{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (forever, when, void)
import Data.IORef
import Data.Word (Word32)
import Foreign hiding (void)
import Foreign.C

import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

data UserState = UserState
  { rbPtr    :: Ptr MaRB
  , channels :: Word32
  , sr       :: Double
  , phaseRef :: IORef Double
  }

main :: IO ()
main = do
  let sampleRate = 48000 :: Word32
      ch         = 2     :: Word32
      rbFrames   = 48000 :: Word32   -- 1 second buffer

  ph <- newIORef 0.0
  rb <- rbCreateF32 rbFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  stStable <- newStablePtr (UserState rb ch (fromIntegral sampleRate) ph)
  let userDataPtr = castStablePtrToPtr stStable

  -- Callback: drain ring buffer -> output, then zero rest.
  let cb :: MaDataCallback
      cb dev pOut _pIn frameCount = do
        ud <- hs_ma_device_get_user_data dev
        if ud == nullPtr
          then fillBytes pOut 0 (fromIntegral frameCount * fromIntegral ch * sizeOf (undefined :: CFloat))
          else do
            UserState rb' ch' _sr' _phRef <- deRefStablePtr (castPtrToStablePtr ud)
            let outF = castPtr pOut :: Ptr CFloat
            got <- rbReadF32 rb' outF frameCount

            let samplesGot  = fromIntegral got * fromIntegral ch'
                samplesWant = fromIntegral frameCount * fromIntegral ch'
                remaining   = samplesWant - samplesGot

            when (remaining > 0) $
              fillBytes (outF `advancePtr` fromIntegral samplesGot) 0 (remaining * sizeOf (undefined :: CFloat))

  cbFun <- mkMaDataCallback cb

  -- Producer thread: generate audio into ring buffer.
  -- This example generates a sine wave into a temporary buffer then writes it.
  _tid <- forkIO $ forever $ do
    UserState rb' ch' sr' phRef <- deRefStablePtr (castPtrToStablePtr userDataPtr)

    -- Try to keep e.g. 20ms worth queued each tick.
    let framesPerChunk = 960 :: Word32  -- 20ms at 48k
        nSamples = fromIntegral framesPerChunk * fromIntegral ch'

    allocaArray nSamples $ \(tmp :: Ptr CFloat) -> do
      ph0 <- readIORef phRef
      let freq = 440.0
          step = 2 * pi * freq / sr'
          chI  = fromIntegral ch' :: Int
          go i phv
            | i >= fromIntegral framesPerChunk = writeIORef phRef phv
            | otherwise = do
                let s = realToFrac (sin phv) :: CFloat
                pokeElemOff tmp (chI*i + 0) s
                pokeElemOff tmp (chI*i + 1) s
                go (i+1) (phv + step)
      go 0 ph0

      -- Write (might write less than requested if buffer full).
      void $ rbWriteF32 rb' tmp framesPerChunk

    threadDelay 5000 -- 5ms; tune as needed

  bracket
    (hs_ma_device_config_init_playback MaFormatF32 ch sampleRate cbFun userDataPtr)
    hs_ma_device_config_free
    (\cfg -> do
        bracketOnError hs_ma_device_malloc hs_ma_device_free $ \dev -> do
          r <- ma_device_init nullPtr cfg dev
          when (r /= MA_SUCCESS) $ error ("ma_device_init failed: " <> show r)

          finally
            (do
               r2 <- ma_device_start dev
               when (r2 /= MA_SUCCESS) $ error ("ma_device_start failed: " <> show r2)
               putStrLn "Ring-buffer playback. Press Enter to stop."
               void getLine
               void (ma_device_stop dev)
            )
            (ma_device_uninit dev)
    )

  freeHaskellFunPtr cbFun
  freeStablePtr stStable
  rbDestroy rb
