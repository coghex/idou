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
import Data.Word (Word32)
import Foreign hiding (void)
import Foreign.C
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)

import qualified Data.Vector.Mutable as MV

import Engine.Core.Thread
import Engine.Core.Queue as Q
import Audio.Types
import Audio.Envelope

import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

--------------------------------------------------------------------------------
-- Voice representation (fast oscillator + ADSR)
--------------------------------------------------------------------------------

data Voice = Voice
  { vY1         ∷ !Float
  , vY2         ∷ !Float
  , vK          ∷ !Float    -- 2*cos(w)
  , vHoldRemain ∷ !Int      -- frames until we trigger release
  , vAmpL       ∷ !Float
  , vAmpR       ∷ !Float
  , vADSR       ∷ !ADSR
  , vEnv        ∷ !EnvState
  }

data AudioState = AudioState
  { stVoices      ∷ !(MV.IOVector Voice)
  , stActiveCount ∷ !Int
  , stMixBuf      ∷ !(ForeignPtr CFloat) -- interleaved stereo
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

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

sendAudio ∷ AudioSystem → AudioMsg → IO ()
sendAudio sys msg = Q.writeQueue (audioQueue (asHandle sys)) msg

startAudioSystem ∷ IO AudioSystem
startAudioSystem = do
  let sr  = 48000 ∷ Word32
      ch  = 2     ∷ Word32
      rbCapacityFrames = 24000 ∷ Word32  -- 0.5 seconds @ 48k
      chunkFrames = 512 ∷ Int
      maxVoices = 256 ∷ Int

  q <- Q.newQueue
  let h = AudioHandle q sr ch

  rb <- rbCreateF32 rbCapacityFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  udStable <- newStablePtr (AudioUserData rb ch)
  let udPtr = castStablePtrToPtr udStable

  let cb ∷ MaDataCallback
      cb dev pOut _pIn frameCount = do
        ud <- hs_ma_device_get_user_data dev
        AudioUserData rb' ch' <- deRefStablePtr (castPtrToStablePtr ud)
        let outF ∷ Ptr CFloat
            outF = castPtr pOut
        got <- rbReadF32 rb' outF frameCount
        when (got < frameCount) $ do
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
  mixBuf <- mallocForeignPtrArray (chunkFrames * 2)
  stRef <- newIORef (AudioState voicesVec 0 mixBuf)

  controlRef <- newIORef ThreadRunning
  tid <- forkIO $ runAudioLoop h controlRef stRef rb chunkFrames

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

--------------------------------------------------------------------------------
-- Audio thread loop
--------------------------------------------------------------------------------

runAudioLoop
  ∷ AudioHandle
  → IORef ThreadControl
  → IORef AudioState
  → Ptr MaRB
  → Int
  → IO ()
runAudioLoop h controlRef stRef rb chunkFrames = go
  where
    go = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> pure ()
        ThreadPaused  -> threadDelay 10000 >> go
        ThreadRunning -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h rb st0
          st2 <- renderIfNeeded h rb chunkFrames st1
          writeIORef stRef st2
          threadDelay 1000
          go

processMsgs ∷ AudioHandle → Ptr MaRB → AudioState → IO AudioState
processMsgs h rb st = do
  m <- Q.tryReadQueue (audioQueue h)
  case m of
    Nothing -> pure st
    Just msg ->
      case msg of
        AudioShutdown -> do
          rbReset rb
          pure st { stActiveCount = 0 }

        AudioStopAll  -> do
          rbReset rb
          processMsgs h rb st { stActiveCount = 0 }

        AudioPlayBeep a p f d e -> do
          st' <- addBeepVoice h st a p f d e
          processMsgs h rb st'

addBeepVoice ∷ AudioHandle → AudioState → Float → Float → Float → Float → ADSR → IO AudioState
addBeepVoice h st amp pan freqHz durSec adsrSpec = do
  let n = stActiveCount st
      vec = stVoices st
      cap = MV.length vec
  if n >= cap
    then pure st
    else do
      let srD = fromIntegral (sampleRate h) ∷ Double
          srF = fromIntegral (sampleRate h) ∷ Float
          holdFrames = max 0 (floor (realToFrac durSec * srD) ∷ Int)

          panClamped = max (-1) (min 1 pan)
          ang = (realToFrac (panClamped + 1) ∷ Double) * (pi/4)
          gL = realToFrac (cos ang) ∷ Float
          gR = realToFrac (sin ang) ∷ Float
          ampL = amp * gL
          ampR = amp * gR

          w = 2*pi*(realToFrac freqHz ∷ Double) / srD
          k = realToFrac (2 * cos w) ∷ Float

          y1 = 0.0 ∷ Float
          y2 = negate (realToFrac (sin w) ∷ Float)

          -- Ensure sustain is clamped; other values handled in envStep.
          adsr' = adsrSpec { aSustain = clamp (aSustain adsrSpec) }
          clamp x | x < 0 = 0 | x > 1 = 1 | otherwise = x

          -- Start in attack at 0
          env0 = envInit

          -- If attack is very slow and you play short sounds, you'll hear the attack ramp.
          _ = srF  -- keep srF around if you later want to precompute per-voice increments

          v = Voice y1 y2 k holdFrames ampL ampR adsr' env0

      MV.write vec n v
      pure st { stActiveCount = n + 1 }

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderIfNeeded ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderIfNeeded h rb chunkFrames st0 = do
  let targetFrames = (sampleRate h `div` 5)  -- 200ms queued

      loop st = do
        availRead  <- rbAvailableRead rb
        availWrite <- rbAvailableWrite rb

        -- Stop when we have enough queued, or no space to write more.
        if availRead >= targetFrames || availWrite == 0
          then pure st
          else do
            let framesNow = min chunkFrames (fromIntegral availWrite)
            st' <- renderChunkFrames h rb framesNow st
            loop st'

  loop st0

renderChunkFrames ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderChunkFrames h rb framesNow st = do
  st' <- withForeignPtr (stMixBuf st) $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
        samples = framesNow * 2
        bytes = samples * sizeOf (undefined ∷ CFloat)
    fillBytes out 0 bytes
    mixVoices (fromIntegral (sampleRate h)) framesNow out st

  withForeignPtr (stMixBuf st') $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
    written <- rbWriteF32 rb out (fromIntegral framesNow)
    -- With looped C write + availWrite gating, this should be full.
    when (written /= fromIntegral framesNow) $
      pure ()  -- optionally track a counter

  pure st'

mixVoices ∷ Float → Int → Ptr CFloat → AudioState → IO AudioState
mixVoices sr chunkFrames out st0 = do
  let vec = stVoices st0
      loop i st
        | i >= stActiveCount st = pure st
        | otherwise = do
            v <- MV.read vec i
            v' <- mixOneVoice sr chunkFrames out v
            if eStage (vEnv v') == EnvDone
              then do
                let lastIx = stActiveCount st - 1
                when (i /= lastIx) $
                  MV.read vec lastIx >>= MV.write vec i
                loop i st { stActiveCount = lastIx }
              else do
                MV.write vec i v'
                loop (i+1) st
  loop 0 st0

mixOneVoice ∷ Float → Int → Ptr CFloat → Voice → IO Voice
mixOneVoice sr chunkFrames out v0 = do
  let framesToDo = chunkFrames
      k    = vK v0
      ampL = vAmpL v0
      ampR = vAmpR v0
      adsrSpec = vADSR v0

      go :: Int -> Ptr CFloat -> Voice -> IO Voice
      go i p v
        | i >= framesToDo = pure v
        | otherwise = do
            -- Trigger release when hold time elapses
            let hold = vHoldRemain v
                env0 = if hold <= 0 then envRelease (vEnv v) else vEnv v
                (env1, lvl) = envStep sr adsrSpec env0

                -- oscillator step
                y0 = vK v * vY1 v - vY2 v

                sL = realToFrac (y0 * (ampL * lvl)) :: CFloat
                sR = realToFrac (y0 * (ampR * lvl)) :: CFloat

            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + sL)
            poke (p `advancePtr` 1) (curR + sR)

            let v' = v
                  { vY2 = vY1 v
                  , vY1 = y0
                  , vEnv = env1
                  , vHoldRemain = hold - 1
                  }

            go (i+1) (p `advancePtr` 2) v'

  go 0 out v0
