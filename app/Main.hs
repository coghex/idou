{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forever)
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
    putStrLn "Terminal input mode:"
    putStrLn "  Press 'c' toggle middle C"
    putStrLn "  Press 'd' toggle D"
    putStrLn "  Press <tab> to switch instrument (0 <-> 1) used by NEW note-ons"
    putStrLn "  Press 'q' to quit"

    let env0 = ADSR
          { aAttackSec  = 0.02
          , aDecaySec   = 0.10
          , aSustain    = 0.6
          , aReleaseSec = 0.50
          }

        inst0 = Instrument { iWaveform = WaveSine, iAdsrDefault = env0, iGain = 1.0 }
        inst1 = Instrument { iWaveform = WaveSaw , iAdsrDefault = env0, iGain = 0.8 }

    sendAudio sys (AudioSetInstrument (InstrumentId 0) inst0)
    sendAudio sys (AudioSetInstrument (InstrumentId 1) inst1)

    currentInstrRef <- newIORef (InstrumentId 0)

    -- Track whether the note is down, and if so, which instrument started it.
    cHeldRef <- newIORef (Nothing ∷ Maybe InstrumentId)
    dHeldRef <- newIORef (Nothing ∷ Maybe InstrumentId)

    let middleC = NoteMidi 60
        dNext   = NoteMidi 62

        noteOn iid nid =
          sendAudio sys (AudioNoteOn iid 0.25 0.0 nid Nothing)

        noteOff iid nid =
          sendAudio sys (AudioNoteOff iid nid)

        toggleNote heldRef nid = do
          held <- readIORef heldRef
          case held of
            Nothing -> do
              iid <- readIORef currentInstrRef
              noteOn iid nid
              writeIORef heldRef (Just iid)
            Just iid -> do
              noteOff iid nid
              writeIORef heldRef Nothing

    forever $ do
      ch <- hGetChar stdin
      case ch of
        'q' -> do
          -- release whatever is currently held (using the instrument that started it)
          mcHeld <- readIORef cHeldRef
          dHeld  <- readIORef dHeldRef
          case mcHeld of
            Nothing  -> pure ()
            Just iid -> noteOff iid middleC
          case dHeld of
            Nothing  -> pure ()
            Just iid -> noteOff iid dNext

          putStrLn "\nQuit."
          exitSuccess

        '\t' -> do
          iid <- readIORef currentInstrRef
          let iid' = if iid == InstrumentId 0 then InstrumentId 1 else InstrumentId 0
          writeIORef currentInstrRef iid'
          putStrLn ("\nInstrument switched to " <> show iid' <> "\n")

        'c' -> toggleNote cHeldRef middleC
        'd' -> toggleNote dHeldRef dNext

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
