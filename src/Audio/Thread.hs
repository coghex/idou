{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread
  ( startAudioSystem
  , stopAudioSystem
  , sendAudio
  , AudioSystem(..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, finally)
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

import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

--------------------------------------------------------------------------------
-- Voice representation (fast oscillator, no sin in render)
--------------------------------------------------------------------------------

-- Sine recurrence:
-- y0 = 2*cos(w)*y1 - y2
-- keep y1,y2 as state. Output y0 each step.
data Voice = Voice
  { vY1     ∷ !Float
  , vY2     ∷ !Float
  , vK      ∷ !Float    -- 2*cos(w)
  , vRemain ∷ !Int      -- frames remaining
  , vAmpL   ∷ !Float
  , vAmpR   ∷ !Float
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
      maxVoices = 256 ∷ Int  -- supports >64 comfortably

  q <- Q.newQueue
  let h = AudioHandle q sr ch

  rb <- rbCreateF32 rbCapacityFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  udStable <- newStablePtr (AudioUserData rb ch)
  let udPtr = castStablePtrToPtr udStable

  -- Callback: drain ringbuffer; silence on underrun.
  let cb ∷ MaDataCallback
      cb _dev pOut _pIn frameCount = do
        AudioUserData rb' ch' <- deRefStablePtr (castPtrToStablePtr udPtr)
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

  -- Engine state
  voicesVec <- MV.new maxVoices
  mixBuf <- mallocForeignPtrArray (chunkFrames * 2) -- stereo interleaved
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
  → Int          -- chunkFrames
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
          st1 <- processMsgs h st0
          st2 <- renderIfNeeded h rb chunkFrames st1
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
        AudioShutdown -> pure st { stActiveCount = 0 }
        AudioStopAll  -> processMsgs h st { stActiveCount = 0 }
        AudioPlayBeep a p f d -> do
          st' <- addBeepVoice h st a p f d
          processMsgs h st'

addBeepVoice ∷ AudioHandle → AudioState → Float → Float → Float → Float → IO AudioState
addBeepVoice h st amp pan freqHz durSec = do
  let n = stActiveCount st
      vec = stVoices st
      cap = MV.length vec
  if n >= cap
    then pure st  -- drop if full (later: voice stealing)
    else do
      let srD = fromIntegral (sampleRate h) ∷ Double
          frames = max 0 (floor (realToFrac durSec * srD) ∷ Int)

          -- pan constant power
          panClamped = max (-1) (min 1 pan)
          ang = (realToFrac (panClamped + 1) ∷ Double) * (pi/4)
          gL = realToFrac (cos ang) ∷ Float
          gR = realToFrac (sin ang) ∷ Float
          ampL = amp * gL
          ampR = amp * gR

          w = 2*pi*(realToFrac freqHz ∷ Double) / srD
          k = realToFrac (2 * cos w) ∷ Float

          -- Initialize recurrence roughly as sin(0)=0, sin(-w)=-sin(w)
          y1 = 0.0 ∷ Float
          y2 = negate (realToFrac (sin w) ∷ Float)

          v = Voice y1 y2 k frames ampL ampR

      MV.write vec n v
      pure st { stActiveCount = n + 1 }

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderIfNeeded ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderIfNeeded h rb chunkFrames st = do
  avail <- rbAvailableRead rb
  let targetFrames = (sampleRate h `div` 10)  -- 100ms buffered
  if avail >= targetFrames
    then pure st
    else renderChunk h rb chunkFrames st

renderChunk ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderChunk h rb chunkFrames st = do
  -- mix into reusable buffer
  st' <- withForeignPtr (stMixBuf st) $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
        samples = chunkFrames * 2
        bytes = samples * sizeOf (undefined ∷ CFloat)
    fillBytes out 0 bytes
    mixVoices chunkFrames out st

  -- write to ring buffer
  withForeignPtr (stMixBuf st') $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
    void (rbWriteF32 rb out (fromIntegral chunkFrames))

  pure st'

mixVoices ∷ Int → Ptr CFloat → AudioState → IO AudioState
mixVoices chunkFrames out st0 = do
  let vec = stVoices st0
  -- iterate voices by index; swap-remove dead voices
  let loop i st
        | i >= stActiveCount st = pure st
        | otherwise = do
            v <- MV.read vec i
            v' <- mixOneVoice chunkFrames out v
            if vRemain v' <= 0
              then do
                -- swap-remove
                let lastIx = stActiveCount st - 1
                if i /= lastIx
                  then MV.read vec lastIx >>= MV.write vec i
                  else pure ()
                loop i st { stActiveCount = lastIx }
              else do
                MV.write vec i v'
                loop (i+1) st
  loop 0 st0

mixOneVoice ∷ Int → Ptr CFloat → Voice → IO Voice
mixOneVoice chunkFrames out v0 = do
  let framesToDo = min chunkFrames (vRemain v0)
      ampL = vAmpL v0
      ampR = vAmpR v0
      k    = vK v0

  let go :: Int -> Ptr CFloat -> Float -> Float -> IO (Float, Float)
      go i p y1 y2
        | i >= framesToDo = pure (y1, y2)
        | otherwise = do
            let y0 = k*y1 - y2
                l  = realToFrac (y0 * ampL) :: CFloat
                r  = realToFrac (y0 * ampR) :: CFloat

            -- p points at L, p+1 is R
            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + l)
            poke (p `advancePtr` 1) (curR + r)

            go (i+1) (p `advancePtr` 2) y0 y1

  (y1', y2') <- go 0 out (vY1 v0) (vY2 v0)
  pure v0 { vY1 = y1', vY2 = y2', vRemain = vRemain v0 - framesToDo }
