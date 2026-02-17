module Main where

import Control.Exception (bracket)
import Audio.Thread
import Audio.Types
import Audio.Envelope

main ∷ IO ()
main =
  bracket startAudioSystem stopAudioSystem $ \sys -> do
    putStrLn "Audio system started."
    putStrLn "Press Enter to play a note with ADSR. Type 'q' then Enter to quit."

    let demoEnv = ADSR
          { aAttackSec  = 0.4
          , aDecaySec   = 0.3
          , aSustain    = 0.4
          , aReleaseSec = 1.0
          }

        loop = do
          line <- getLine
          if line == "q"
            then pure ()
            else do
              -- durSec is hold time before release
              sendAudio sys (AudioPlayBeep 0.25 0.0 440 0.6 demoEnv)
              loop

    loop
