{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread
  ( startAudioSystem
  , stopAudioSystem
  , sendAudio
  , AudioSystem(..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (when, void)
import Data.IORef
import Data.Word (Word32)
import Foreign hiding (void)
import Foreign.C
import Foreign.Marshal.Utils (fillBytes)

import Engine.Core.Thread
import Engine.Core.Queue as Q
import Audio.Types

import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

data Voice = Beep
  { vPhase     ∷ !Double
  , vStep      ∷ !Double
  , vRemain    ∷ !Int        -- frames remaining
  , vAmp       ∷ !Float
  , vPan       ∷ !Float
  }

data AudioState = AudioState
  { voices ∷ ![Voice]
  }

data AudioSystem = AudioSystem
  { asHandle     ∷ !AudioHandle
  , asThread     ∷ !ThreadState
  , asDevice     ∷ !(Ptr MaDevice)
  , asCfg        ∷ !(Ptr MaDeviceConfig)
  , asCallback   ∷ !(FunPtr MaDataCallback)
  , asUserData   ∷ !(StablePtr AudioUserData)
  , asRingBuffer ∷ !(Ptr MaRB)
  }

data AudioUserData = AudioUserData
  { aud_rb       ∷ !(Ptr MaRB)
  , aud_channels ∷ !Word32
  }

sendAudio ∷ AudioSystem → AudioMsg → IO ()
sendAudio sys msg = Q.writeQueue (audioQueue (asHandle sys)) msg

startAudioSystem ∷ IO AudioSystem
startAudioSystem = do
  let sr  = 48000 ∷ Word32
      ch  = 2     ∷ Word32
      rbCapacityFrames = 24000 ∷ Word32  -- 0.5 seconds @ 48k

  q <- Q.newQueue
  let h = AudioHandle q sr ch

  rb <- rbCreateF32 rbCapacityFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  -- user data for callback
  udStable <- newStablePtr (AudioUserData rb ch)
  let udPtr :: Ptr ()
      udPtr = castStablePtrToPtr udStable

  -- callback: drain ringbuffer -> output, else silence
  let cb :: MaDataCallback
      cb _dev pOut _pIn frameCount = do
        AudioUserData rb' ch' <- deRefStablePtr (castPtrToStablePtr udPtr)
        let outF :: Ptr CFloat
            outF = castPtr pOut

        got <- rbReadF32 rb' outF frameCount
        when (got < frameCount) $ do
          let samplesGot  = fromIntegral got * fromIntegral ch'
              samplesWant = fromIntegral frameCount * fromIntegral ch'
              remainingSamples = samplesWant - samplesGot
              byteCount = remainingSamples * sizeOf (undefined :: CFloat)
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

  -- audio engine thread state
  controlRef <- newIORef ThreadRunning
  stRef <- newIORef (AudioState [])
  tid <- forkIO $ runAudioLoop h controlRef stRef rb

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
  -- stop engine thread first (so it stops writing)
  writeIORef (tsRunning (asThread sys)) ThreadStopped
  -- optionally ask it to shutdown too
  Q.writeQueue (audioQueue (asHandle sys)) AudioShutdown
  threadDelay 20000

  -- stop device + cleanup
  void (ma_device_stop (asDevice sys))
  ma_device_uninit (asDevice sys)

  hs_ma_device_free (asDevice sys)
  hs_ma_device_config_free (asCfg sys)

  freeHaskellFunPtr (asCallback sys)
  freeStablePtr (asUserData sys)

  rbDestroy (asRingBuffer sys)

runAudioLoop ∷ AudioHandle → IORef ThreadControl → IORef AudioState → Ptr MaRB → IO ()
runAudioLoop h controlRef stRef rb = go
  where
    go = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> pure ()
        ThreadPaused  -> threadDelay 10000 >> go
        ThreadRunning -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h st0
          st2 <- renderIfNeeded h rb st1
          writeIORef stRef st2
          threadDelay 1000
          go

processMsgs ∷ AudioHandle → AudioState → IO AudioState
processMsgs h st = do
  m <- Q.tryReadQueue (audioQueue h)
  case m of
    Nothing -> pure st
    Just msg ->
      case msg of
        AudioShutdown -> pure st { voices = [] }
        AudioStopAll  -> processMsgs h st { voices = [] }
        AudioPlayBeep a p f d -> do
          let srD = fromIntegral (sampleRate h) :: Double
              frames = max 0 (floor (realToFrac d * srD) :: Int)
              step = 2*pi*(realToFrac f :: Double) / srD
              v = Beep 0 step frames a p
          processMsgs h st { voices = v : voices st }

renderIfNeeded ∷ AudioHandle → Ptr MaRB → AudioState → IO AudioState
renderIfNeeded h rb st = do
  -- maintain at least ~100ms buffered
  avail <- rbAvailableRead rb
  let targetFrames = (sampleRate h `div` 10)  -- 0.1 sec
  if avail >= targetFrames
    then pure st
    else renderChunk h rb 512 st

renderChunk ∷ AudioHandle → Ptr MaRB → Word32 → AudioState → IO AudioState
renderChunk h rb framesWanted st = do
  let ch = channels h
      nFrames = fromIntegral framesWanted :: Int
      nSamples = nFrames * fromIntegral ch

  allocaArray nSamples $ \(tmp :: Ptr CFloat) -> do
    -- mix into tmp
    st' <- mixVoices h framesWanted tmp st
    void (rbWriteF32 rb tmp framesWanted)
    pure st'

mixVoices ∷ AudioHandle → Word32 → Ptr CFloat → AudioState → IO AudioState
mixVoices h framesWanted out st0 = do
  let chI = fromIntegral (channels h) :: Int
      nFrames = fromIntegral framesWanted :: Int
      nSamples = nFrames * chI

  -- clear output
  fillBytes out 0 (nSamples * sizeOf (undefined :: CFloat))

  -- mix each voice
  (vs', _) <- foldlM (mixOne chI nFrames out) ([], 0::Int) (voices st0)
  pure st0 { voices = reverse vs' }

mixOne
  ∷ Int → Int → Ptr CFloat
  → ([Voice], Int) → Voice → IO ([Voice], Int)
mixOne chI nFrames out (acc, _) v = do
  case v of
    Beep ph step remain amp panVal -> do
      let framesToDo = min nFrames remain
          -- constant-power pan
          panClamped = max (-1) (min 1 panVal)
          ang = (realToFrac (panClamped + 1) :: Double) * (pi/4)
          gL = realToFrac (cos ang) :: Float
          gR = realToFrac (sin ang) :: Float
          aL = amp * gL
          aR = amp * gR

      let go i ph' = when (i < framesToDo) $ do
            let s = realToFrac (sin ph') :: Float
                l = realToFrac (s * aL) :: CFloat
                r = realToFrac (s * aR) :: CFloat
                ix = chI*i
            curL <- peekElemOff out (ix + 0)
            curR <- peekElemOff out (ix + 1)
            pokeElemOff out (ix + 0) (curL + l)
            pokeElemOff out (ix + 1) (curR + r)
            go (i+1) (ph' + step)

      go 0 ph

      let remain' = remain - framesToDo
          ph' = ph + fromIntegral framesToDo * step
      if remain' > 0
        then pure (Beep ph' step remain' amp panVal : acc, 0)
        else pure (acc, 0)

foldlM ∷ Monad m ⇒ (a → b → m a) → a → [b] → m a
foldlM _ z [] = pure z
foldlM f z (x:xs) = f z x >>= \z' -> foldlM f z' xs
