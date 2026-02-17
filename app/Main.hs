{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (when, void)
import Data.IORef
import Foreign hiding (void)
import Foreign.C
import Sound.Miniaudio

-- We'll get the user data from the device via a tiny helper in C in the next step.
-- For now, keep it captured (works), but fix the StablePtr cast properly:
castPtrToStable :: Ptr () -> StablePtr a
castPtrToStable = castPtrToStablePtr

data UserState = UserState
  { phase :: IORef Double
  , sr    :: Double
  }

main :: IO ()
main = do
  let sampleRate :: Word32
      sampleRate = 48000
      channels :: Word32
      channels = 2

  ph <- newIORef 0.0
  stStable <- newStablePtr (UserState ph (fromIntegral sampleRate))

  let userDataPtr :: Ptr ()
      userDataPtr = castStablePtrToPtr stStable

  let cb :: MaDataCallback
      cb _dev pOut _pIn frameCount = do
        -- Audio thread: keep it simple and fast.
        UserState phRef sr' <- deRefStablePtr (castPtrToStable userDataPtr)

        let outF :: Ptr CFloat
            outF = castPtr pOut
            nFrames = fromIntegral frameCount :: Int
            freq = 440.0 :: Double
            step = 2 * pi * freq / sr'

        ph0 <- readIORef phRef
        let chI = fromIntegral channels :: Int
            go i phv
              | i >= nFrames = writeIORef phRef phv
              | otherwise = do
                  let s = realToFrac (sin phv) :: CFloat
                  pokeElemOff outF (chI*i + 0) s
                  pokeElemOff outF (chI*i + 1) s
                  go (i+1) (phv + step)
        go 0 ph0

  cbFun <- mkMaDataCallback cb

  bracket
    (hs_ma_device_config_init_playback MaFormatF32 channels sampleRate cbFun userDataPtr)
    hs_ma_device_config_free
    (\cfg -> do
        when (cfg == nullPtr) $
          error "Failed to allocate ma_device_config"

        bracketOnError hs_ma_device_malloc hs_ma_device_free $ \dev -> do
          r <- ma_device_init nullPtr cfg dev
          when (r /= MA_SUCCESS) $
            error ("ma_device_init failed with code " <> show r)

          finally
            (do
               r2 <- ma_device_start dev
               when (r2 /= MA_SUCCESS) $
                 error ("ma_device_start failed with code " <> show r2)

               putStrLn "Playing a sine wave. Press Enter to stop."
               void getLine

               void (ma_device_stop dev)
            )
            (ma_device_uninit dev)
    )

  freeHaskellFunPtr cbFun
  freeStablePtr stStable
