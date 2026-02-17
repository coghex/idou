{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Audio.Thread
import Audio.Types

main ∷ IO ()
main =
  bracket startAudioSystem stopAudioSystem $ \sys -> do
    putStrLn "Audio system started."
    putStrLn "Press Enter to play a 1s beep. Type 'q' then Enter to quit."

    let loop = do
          line <- getLine
          if line == "q"
            then pure ()
            else do
              -- minimal test message: 1 second beep
              sendAudio sys (AudioPlayBeep 0.2 0.0 440 1.0)
              loop
    loop
