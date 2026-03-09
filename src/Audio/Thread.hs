{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread
  ( startAudioSystem
  , stopAudioSystem
  , sendAudio
  , AudioHealthVerbosity(..)
  , AudioSystem(..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, onException)
import Control.Monad (when, void)
import Data.IORef
import Foreign hiding (void)
import Foreign.C

import qualified Data.Vector.Mutable as MV

import Audio.Config (AudioConfig(..), AudioBufferConfig(..))
import Engine.Core.Thread
import Engine.Core.Queue as Q
import Audio.Types
import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

import Audio.Thread.Types
import Audio.Thread.InstrumentTable
import Audio.Thread.Voice
import Audio.Thread.Render

data AudioSystem = AudioSystem
  { asHandle     ∷ !AudioHandle
  , asThread     ∷ !ThreadState
  , asDevice     ∷ !(Ptr MaDevice)
  , asCfg        ∷ !(Ptr MaDeviceConfig)
  , asCallback   ∷ !(FunPtr MaDataCallback)
  , asUserData   ∷ !(StablePtr AudioUserData)
  , asRingBuffer ∷ !(Ptr MaRB)
  }

data AudioHealthVerbosity
  = AudioHealthOff
  | AudioHealthNormal
  | AudioHealthVerbose
  deriving (Eq, Show)

sendAudio ∷ AudioSystem → AudioMsg → IO ()
sendAudio sys msg = Q.writeQueue (audioQueue (asHandle sys)) msg

startAudioSystem ∷ AudioConfig → AudioHealthVerbosity → IO AudioSystem
startAudioSystem audioCfg healthVerbosity = do
  let ch  = 2 ∷ Word32
      sampleRate = acSampleRate audioCfg
      chunkFrames = acChunkFrames audioCfg
      bufferCfg = acBuffer audioCfg
      rbCapacityFrames = abCapacityFrames bufferCfg
      targetBufferFrames = fromIntegral (chunkFrames * abTargetChunks bufferCfg) ∷ Word32
      maxVoices = acMaxVoices audioCfg

  q <- Q.newQueue
  let h = AudioHandle q sampleRate ch
  rb <- createRingBuffer rbCapacityFrames ch
  (underrunRef, udStable) <- (`onException` rbDestroy rb) $ do
    underrunRef <- newIORef 0
    udStable <- newStablePtr (AudioUserData rb ch underrunRef)
    pure (underrunRef, udStable)
  let udPtr = castStablePtrToPtr udStable
      cleanupStable = freeStablePtr udStable >> rbDestroy rb

      cb ∷ MaDataCallback
      cb dev pOut _pIn frameCount = do
        ud <- hs_ma_device_get_user_data dev
        AudioUserData rb' ch' uref <- deRefStablePtr (castPtrToStablePtr ud)
        let outF ∷ Ptr CFloat
            outF = castPtr pOut
        got <- rbReadF32 rb' outF frameCount
        when (got < frameCount) $ do
          modifyIORef' uref (+1)
          let samplesGot  = fromIntegral got * fromIntegral ch'
              samplesWant = fromIntegral frameCount * fromIntegral ch'
              remainingSamples = samplesWant - samplesGot
              byteCount = remainingSamples * sizeOf (undefined ∷ CFloat)
          fillBytes (outF `advancePtr` fromIntegral samplesGot) 0 byteCount

  cbFun <- mkMaDataCallback cb `onException` cleanupStable
  let cleanupCallback = freeHaskellFunPtr cbFun >> cleanupStable

  cfg <- createDeviceConfig ch sampleRate cbFun udPtr `onException` cleanupCallback
  let cleanupConfig = hs_ma_device_config_free cfg >> cleanupCallback

  dev <- createDevice `onException` cleanupConfig
  let cleanupAllocatedDevice = hs_ma_device_free dev >> cleanupConfig
      cleanupInitializedDevice = ma_device_uninit dev >> cleanupAllocatedDevice
      cleanupStartedSystem = void (ma_device_stop dev) >> cleanupInitializedDevice

  initDevice cfg dev `onException` cleanupAllocatedDevice

  (`onException` cleanupInitializedDevice) $ do
    voicesVec <- MV.new maxVoices
    instVec <- MV.replicate 256 Nothing
    glideVec <- MV.replicate 256 0
    legFiltRetrigVec <- MV.replicate 256 False
    legAmpRetrigVec <- MV.replicate 256 False
    vibRateVec <- MV.replicate 256 0
    vibDepthVec <- MV.replicate 256 0
    channelVolumeVec <- MV.replicate 256 1
    channelExpressionVec <- MV.replicate 256 1
    channelPanVec <- MV.replicate 256 0
    modWheelVec <- MV.replicate 256 0
    channelAftertouchVec <- MV.replicate 256 0
    pitchBendVec <- MV.replicate 256 0
    pitchModScratch <- MV.replicate maxLayers 1
    pitchCentsScratch <- MV.replicate maxLayers 0

    mixBuf <- mallocForeignPtrArray (chunkFrames * 2)
    let st0 =
          AudioState
            { stVoices = voicesVec
            , stActiveCount = 0
            , stMixBuf = mixBuf
            , stSampleRate = sampleRate
            , stTargetBufferFrames = targetBufferFrames
            , stPitchModScratch = pitchModScratch
            , stPitchCentsScratch = pitchCentsScratch
            , stInstruments = instVec
            , stGlideSec = glideVec
            , stLegFiltRetrig = legFiltRetrigVec
            , stLegAmpRetrig = legAmpRetrigVec
            , stVibRateHz = vibRateVec
            , stVibDepthCents = vibDepthVec
            , stChannelVolume = channelVolumeVec
            , stChannelExpression = channelExpressionVec
            , stChannelPan = channelPanVec
            , stModWheel = modWheelVec
            , stChannelAftertouch = channelAftertouchVec
            , stPitchBendSemis = pitchBendVec
            , stNow = 0
            }
    (st1, _) <- Audio.Thread.Render.renderIfNeeded rb chunkFrames st0
    stRef <- newIORef st1

    controlRef <- newIORef ThreadRunning
    doneVar <- newEmptyMVar

    (`onException` cleanupStartedSystem) $ do
      startDevice dev
      tid <- forkIO $
        runAudioLoop h controlRef stRef rb chunkFrames underrunRef healthVerbosity
          `finally` putMVar doneVar ()

      let ts = ThreadState controlRef tid doneVar
      pure AudioSystem
        { asHandle = h
        , asThread = ts
        , asDevice = dev
        , asCfg = cfg
        , asCallback = cbFun
        , asUserData = udStable
        , asRingBuffer = rb
        }

createRingBuffer ∷ Word32 → Word32 → IO (Ptr MaRB)
createRingBuffer capacityFrames channels = do
  rb <- rbCreateF32 capacityFrames channels
  when (rb == nullPtr) $ error "rbCreateF32 failed"
  pure rb

createDeviceConfig
  ∷ Word32
  → Word32
  → FunPtr MaDataCallback
  → Ptr ()
  → IO (Ptr MaDeviceConfig)
createDeviceConfig channels sampleRate cbFun udPtr = do
  cfg <- hs_ma_device_config_init_playback MaFormatF32 channels sampleRate cbFun udPtr
  when (cfg == nullPtr) $ error "Failed to allocate ma_device_config"
  pure cfg

createDevice ∷ IO (Ptr MaDevice)
createDevice = do
  dev <- hs_ma_device_malloc
  when (dev == nullPtr) $ error "Failed to allocate ma_device"
  pure dev

initDevice ∷ Ptr MaDeviceConfig → Ptr MaDevice → IO ()
initDevice cfg dev = do
  r <- ma_device_init nullPtr cfg dev
  when (r /= MA_SUCCESS) $ error ("ma_device_init failed: " <> show r)

startDevice ∷ Ptr MaDevice → IO ()
startDevice dev = do
  r <- ma_device_start dev
  when (r /= MA_SUCCESS) $ error ("ma_device_start failed: " <> show r)

stopAudioSystem ∷ AudioSystem → IO ()
stopAudioSystem sys = do
  Q.writeQueue (audioQueue (asHandle sys)) AudioShutdown
  writeIORef (tsRunning (asThread sys)) ThreadStopped
  takeMVar (tsDone (asThread sys))

  void (ma_device_stop (asDevice sys))
  ma_device_uninit (asDevice sys)

  hs_ma_device_free (asDevice sys)
  hs_ma_device_config_free (asCfg sys)

  freeHaskellFunPtr (asCallback sys)
  freeStablePtr (asUserData sys)

  rbDestroy (asRingBuffer sys)

runAudioLoop
  ∷ AudioHandle
  → IORef ThreadControl
  → IORef AudioState
  → Ptr MaRB
  → Int
  → IORef Word64
  → AudioHealthVerbosity
  → IO ()
runAudioLoop h controlRef stRef rb chunkFrames underrunRef healthVerbosity = go 0 0 0 emptyHealthStats
  where
    go ∷ Word64 → Word64 → Word64 → HealthStats → IO ()
    go lastUnderruns lastPartialWrites lastZeroWrites health = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h rb st0
          writeIORef stRef st1
        ThreadPaused  -> threadDelay 10000 >> go lastUnderruns lastPartialWrites lastZeroWrites health
        ThreadRunning -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h rb st0
          (st2, rs) <- Audio.Thread.Render.renderIfNeeded rb chunkFrames st1
          writeIORef stRef st2

          let health' = absorbRenderStats health rs
          u <- readIORef underrunRef
          let partialNow = hsPartialWrites health'
              zeroNow = hsZeroWrites health'
              changed = u /= lastUnderruns || partialNow /= lastPartialWrites || zeroNow /= lastZeroWrites
              shouldLog =
                case healthVerbosity of
                  AudioHealthOff -> False
                  AudioHealthNormal -> changed
                  AudioHealthVerbose ->
                    changed || (hsLoopCount health' `mod` verboseReportEveryLoops == 0)

          when shouldLog $
            emitHealthLog h rb u health'

          threadDelay 1000
          go u partialNow zeroNow health'

data HealthStats = HealthStats
  { hsLoopCount     ∷ !Word64
  , hsFramesMixed   ∷ !Word64
  , hsFramesWritten ∷ !Word64
  , hsPartialWrites ∷ !Word64
  , hsZeroWrites    ∷ !Word64
  } deriving (Eq, Show)

emptyHealthStats ∷ HealthStats
emptyHealthStats =
  HealthStats
    { hsLoopCount = 0
    , hsFramesMixed = 0
    , hsFramesWritten = 0
    , hsPartialWrites = 0
    , hsZeroWrites = 0
    }

verboseReportEveryLoops ∷ Word64
verboseReportEveryLoops = 1000

absorbRenderStats ∷ HealthStats → RenderStats → HealthStats
absorbRenderStats hs rs =
  hs
    { hsLoopCount = hsLoopCount hs + 1
    , hsFramesMixed = hsFramesMixed hs + fromIntegral (rsFramesMixed rs)
    , hsFramesWritten = hsFramesWritten hs + fromIntegral (rsFramesWritten rs)
    , hsPartialWrites = hsPartialWrites hs + fromIntegral (rsPartialWrites rs)
    , hsZeroWrites = hsZeroWrites hs + fromIntegral (rsZeroWrites rs)
    }

emitHealthLog ∷ AudioHandle → Ptr MaRB → Word64 → HealthStats → IO ()
emitHealthLog h rb underruns hs = do
  qDepth <- Q.queueLength (audioQueue h)
  availRead <- rbAvailableRead rb
  availWrite <- rbAvailableWrite rb
  let rbTotal = availRead + availWrite
      rbFillPct ∷ Double
      rbFillPct =
        if rbTotal == 0
          then 0
          else (fromIntegral availRead * 100) / fromIntegral rbTotal
  putStrLn $
    "[audio][health] underruns=" <> show underruns
      <> " qDepth=" <> show qDepth
      <> " rbFill=" <> show (realToFrac rbFillPct ∷ Float) <> "%"
      <> " mixed=" <> show (hsFramesMixed hs)
      <> " written=" <> show (hsFramesWritten hs)
      <> " partialWrites=" <> show (hsPartialWrites hs)
      <> " zeroWrites=" <> show (hsZeroWrites hs)

processMsgs ∷ AudioHandle → Ptr MaRB → AudioState → IO AudioState
processMsgs h rb st = do
  m <- Q.tryReadQueue (audioQueue h)
  case m of
    Nothing -> pure st
    Just msg ->
      case msg of
        AudioShutdown -> rbReset rb >> pure st { stActiveCount = 0 }
        AudioStopAll  -> rbReset rb >> processMsgs h rb st { stActiveCount = 0 }

        AudioLoadInstrument iid inst ->
          setInstrument iid inst st >>= processMsgs h rb

        AudioSetInstrument iid inst -> do
          st'  <- setInstrument iid inst st
          st'' <- applyInstrumentToActiveVoices (sampleRate h) iid inst st'
          processMsgs h rb st''

        AudioSetGlideSec iid gs -> setGlide iid gs st >>= processMsgs h rb
        AudioSetLegatoFilterRetrig iid rf -> setLegatoFilterRetrig iid rf st >>= processMsgs h rb
        AudioSetLegatoAmpRetrig iid ra -> setLegatoAmpRetrig iid ra st >>= processMsgs h rb
        AudioSetVibrato iid rate depth -> setVibrato iid rate depth st >>= processMsgs h rb
        AudioSetChannelVolume iid vol -> setChannelVolume iid vol st >>= processMsgs h rb
        AudioSetChannelPan iid pan -> setChannelPan iid pan st >>= processMsgs h rb
        AudioSetExpression iid expr -> setExpression iid expr st >>= processMsgs h rb
        AudioSetModWheel iid mw -> setModWheel iid mw st >>= processMsgs h rb
        AudioSetChannelAftertouch iid aft -> setChannelAftertouch iid aft st >>= processMsgs h rb
        AudioSetPitchBend iid semis -> setPitchBend iid semis st >>= processMsgs h rb
        AudioResetControllers iid -> resetMidiControls iid st >>= processMsgs h rb

        AudioNoteOffInstrument iid -> releaseInstrumentAllVoices iid st >>= processMsgs h rb
        AudioPlayBeep a p f d e -> addBeepVoice h st a p f d e >>= processMsgs h rb

        -- UPDATED: MIDI-native NoteOn/NoteOff
        AudioNoteOn iid a p key instId vel eOverride ->
          addInstrumentNote h st iid a p key instId vel eOverride >>= processMsgs h rb

        AudioNoteOff iid instId ->
          releaseInstrumentNote iid instId st >>= processMsgs h rb

        AudioSetNoteAftertouch iid instId aft ->
          setInstrumentNoteAftertouch iid instId aft st >>= processMsgs h rb
