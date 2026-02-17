{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread
  ( startAudioSystem
  , stopAudioSystem
  , sendAudio
  , AudioSystem(..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Data.IORef
import Data.Word (Word32, Word64)
import Foreign hiding (void)
import Foreign.C
import Foreign.ForeignPtr (mallocForeignPtrArray)
import Foreign.Marshal.Utils (fillBytes)

import qualified Data.Vector.Mutable as MV

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

sendAudio ∷ AudioSystem → AudioMsg → IO ()
sendAudio sys msg = Q.writeQueue (audioQueue (asHandle sys)) msg

startAudioSystem ∷ IO AudioSystem
startAudioSystem = do
  let ch  = 2     ∷ Word32
      chunkFrames = 256 ∷ Int
      rbCapacityFrames = 8192 ∷ Word32
      maxVoices = 256 ∷ Int

  q <- Q.newQueue
  let h = AudioHandle q sr ch

  rb <- rbCreateF32 rbCapacityFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  underrunRef <- newIORef 0
  udStable <- newStablePtr (AudioUserData rb ch underrunRef)
  let udPtr = castStablePtrToPtr udStable

  let cb ∷ MaDataCallback
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

  cbFun <- mkMaDataCallback cb

  cfg <- hs_ma_device_config_init_playback MaFormatF32 ch sr cbFun udPtr
  when (cfg == nullPtr) $ error "Failed to allocate ma_device_config"

  dev <- hs_ma_device_malloc
  when (dev == nullPtr) $ error "Failed to allocate ma_device"

  r <- ma_device_init nullPtr cfg dev
  when (r /= MA_SUCCESS) $ error ("ma_device_init failed: " <> show r)

  r2 <- ma_device_start dev
  when (r2 /= MA_SUCCESS) $ error ("ma_device_start failed: " <> show r2)

  voicesVec <- MV.new maxVoices
  instVec <- MV.replicate 256 Nothing
  glideVec <- MV.replicate 256 0
  legFiltRetrigVec <- MV.replicate 256 False
  legAmpRetrigVec <- MV.replicate 256 False
  vibRateVec <- MV.replicate 256 0
  vibDepthVec <- MV.replicate 256 0

  mixBuf <- mallocForeignPtrArray (chunkFrames * 2)
  stRef <- newIORef (AudioState voicesVec 0 mixBuf instVec glideVec legFiltRetrigVec legAmpRetrigVec vibRateVec vibDepthVec 0)

  controlRef <- newIORef ThreadRunning
  tid <- forkIO $ runAudioLoop h controlRef stRef rb chunkFrames underrunRef

  let ts = ThreadState controlRef tid
  pure AudioSystem
    { asHandle = h
    , asThread = ts
    , asDevice = dev
    , asCfg = cfg
    , asCallback = cbFun
    , asUserData = udStable
    , asRingBuffer = rb
    }

stopAudioSystem ∷ AudioSystem → IO ()
stopAudioSystem sys = do
  writeIORef (tsRunning (asThread sys)) ThreadStopped
  Q.writeQueue (audioQueue (asHandle sys)) AudioShutdown
  threadDelay 20000

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
  → IO ()
runAudioLoop h controlRef stRef rb chunkFrames underrunRef = go 0
  where
    go ∷ Word64 → IO ()
    go lastPrinted = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> pure ()
        ThreadPaused  -> threadDelay 10000 >> go lastPrinted
        ThreadRunning -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h rb st0
          st2 <- Audio.Thread.Render.renderIfNeeded rb chunkFrames st1
          writeIORef stRef st2

          u <- readIORef underrunRef
          when (u /= lastPrinted) $ do
            putStrLn ("[audio] underruns: " <> show u)
            threadDelay 1000000
            go u

          threadDelay 1000
          go lastPrinted

processMsgs ∷ AudioHandle → Ptr MaRB → AudioState → IO AudioState
processMsgs h rb st = do
  m <- Q.tryReadQueue (audioQueue h)
  case m of
    Nothing -> pure st
    Just msg ->
      case msg of
        AudioShutdown -> rbReset rb >> pure st { stActiveCount = 0 }
        AudioStopAll  -> rbReset rb >> processMsgs h rb st { stActiveCount = 0 }

        AudioSetInstrument iid inst -> do
          st'  <- setInstrument iid inst st
          st'' <- applyInstrumentToActiveVoices (sampleRate h) iid inst st'
          processMsgs h rb st''

        AudioSetGlideSec iid gs -> setGlide iid gs st >>= processMsgs h rb
        AudioSetLegatoFilterRetrig iid rf -> setLegatoFilterRetrig iid rf st >>= processMsgs h rb
        AudioSetLegatoAmpRetrig iid ra -> setLegatoAmpRetrig iid ra st >>= processMsgs h rb
        AudioSetVibrato iid rate depth -> setVibrato iid rate depth st >>= processMsgs h rb

        AudioNoteOffInstrument iid -> releaseInstrumentAllVoices iid st >>= processMsgs h rb
        AudioPlayBeep a p f d e -> addBeepVoice h st a p f d e >>= processMsgs h rb
        AudioNoteOn iid a p nid eOverride -> addInstrumentNote h st iid a p nid eOverride >>= processMsgs h rb
        AudioNoteOff iid nid -> releaseInstrumentNote iid nid st >>= processMsgs h rb
