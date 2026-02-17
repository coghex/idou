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

import Audio.Filter.Types
import Audio.Filter.Biquad (FilterType(..))

main ∷ IO ()
main =
  bracket startAudioSystem stopAudioSystem $ \sys ->
  withRawStdin $ do
    putStrLn "Keys:"
    putStrLn "  c / d   : toggle notes"
    putStrLn "  <tab>   : switch instrument used for NEW note-ons (0<->1)"
    putStrLn "  1 / 2 / 3: select filter preset (updates instruments)"
    putStrLn "  q       : quit"

    let env0 = ADSR
          { aAttackSec  = 0.02
          , aDecaySec   = 0.10
          , aSustain    = 0.6
          , aReleaseSec = 0.50
          }

        -- Filter presets
        presetFilter ∷ Char → Maybe FilterSpec
        presetFilter k =
          case k of
            '1' -> Nothing
            '2' -> Just FilterSpec
                    { fType     = FLP
                    , fCutoffHz = 1200
                    , fQ        = 0.707
                    , fSlope    = S24
                    , fKeyTrack = KeyTrack 0.30
                    }
            '3' -> Just FilterSpec
                    { fType     = FHP
                    , fCutoffHz = 200
                    , fQ        = 0.707
                    , fSlope    = S12
                    , fKeyTrack = KeyTrack 0.0
                    }
            _   -> Nothing

    filterRef <- newIORef (Nothing ∷ Maybe FilterSpec)

    let mkInst wf gain mFilt =
          Instrument
            { iWaveform = wf
            , iAdsrDefault = env0
            , iGain = gain
            , iFilter = mFilt
            }

        pushInstruments mFilt = do
          let inst0 = mkInst WaveSine     1.0 mFilt
              inst1 = mkInst WaveSaw      0.8 mFilt
              inst2 = mkInst WaveSquare   0.8 mFilt
              inst3 = mkInst WaveTriangle 1.0 mFilt
          sendAudio sys (AudioSetInstrument (InstrumentId 0) inst0)
          sendAudio sys (AudioSetInstrument (InstrumentId 1) inst1)
          sendAudio sys (AudioSetInstrument (InstrumentId 2) inst2)
          sendAudio sys (AudioSetInstrument (InstrumentId 3) inst3)

    -- initial instrument setup: no filter
    pushInstruments Nothing

    currentInstrRef <- newIORef (InstrumentId 0)

    -- Track which instrument started each note so NoteOff always matches.
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
          InstrumentId iid <- readIORef currentInstrRef
          let iid' = InstrumentId $ (iid + 1) `mod` 4
          writeIORef currentInstrRef iid'
          putStrLn ("\nInstrument switched to " <> show iid' <> "\n")

        '1' -> do
          let m = presetFilter '1'
          writeIORef filterRef m
          pushInstruments m
          putStrLn "\nFilter preset 1 (off)\n"

        '2' -> do
          let m = presetFilter '2'
          writeIORef filterRef m
          pushInstruments m
          putStrLn "\nFilter preset 2 (LP 1200Hz Q0.707 S24 KT0.3)\n"

        '3' -> do
          let m = presetFilter '3'
          writeIORef filterRef m
          pushInstruments m
          putStrLn "\nFilter preset 3 (HP 200Hz Q0.707 S12)\n"

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
