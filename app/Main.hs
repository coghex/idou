{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.IORef
import System.Exit (exitSuccess)
import System.IO

import Audio.Thread
import Audio.Types
import Audio.Envelope

main ∷ IO ()
main =
  bracket startAudioSystem stopAudioSystem $ \sys ->
  withRawStdin $ do
    putStrLn "Terminal input mode (toggle):"
    putStrLn "  Press 'c' to toggle middle C on/off"
    putStrLn "  Press 'd' to toggle D on/off"
    putStrLn "  Press 'q' to quit"

    let env = ADSR
          { aAttackSec  = 0.02
          , aDecaySec   = 0.10
          , aSustain    = 0.6
          , aReleaseSec = 0.50
          }

        noteOn nid  = sendAudio sys (AudioNoteOn 0.25 0.0 nid env)
        noteOff nid = sendAudio sys (AudioNoteOff nid)

        middleC = NoteMidi 60
        dNext   = NoteMidi 62

    cDownRef <- newIORef False
    dDownRef <- newIORef False

    forever $ do
      ch <- hGetChar stdin
      case ch of
        'q' -> do
          -- ensure everything released
          noteOff middleC
          noteOff dNext
          putStrLn "\nQuit."
          exitSuccess

        'c' -> do
          down <- readIORef cDownRef
          if down
            then noteOff middleC >> writeIORef cDownRef False
            else noteOn  middleC >> writeIORef cDownRef True

        'd' -> do
          down <- readIORef dDownRef
          if down
            then noteOff dNext >> writeIORef dDownRef False
            else noteOn  dNext >> writeIORef dDownRef True

        _ -> pure ()

withRawStdin ∷ IO a → IO a
withRawStdin action = do
  oldEcho <- hGetEcho stdin
  oldBuf  <- hGetBuffering stdin
  bracket
    (do hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        pure (oldEcho, oldBuf))
    (\(e,b) -> do
        hSetEcho stdin e
        hSetBuffering stdin b)
    (\_ -> action)
