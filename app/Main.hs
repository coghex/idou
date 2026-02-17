{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.IORef
import System.Exit (exitSuccess)
import System.IO

import Audio.Thread
import Audio.Types
import Audio.Envelope

import Audio.Filter.Types

main ∷ IO ()
main =
  bracket startAudioSystem stopAudioSystem $ \sys ->
  withRawStdin $ do
    putStrLn "Terminal synth test:"
    putStrLn "  c / d    : toggle notes"
    putStrLn "  <tab>    : switch instrument used for NEW note-ons (0<->1)"
    putStrLn "  1 / 2 / 3: select filter preset (updates instruments)"
    putStrLn "  p        : play plucky sound (one-shot) on both notes"
    putStrLn "  q        : quit"

    let envAmp = ADSR
          { aAttackSec  = 0.02
          , aDecaySec   = 0.10
          , aSustain    = 0.6
          , aReleaseSec = 0.50
          }

        -- Filter presets (these will be applied to instruments via AudioSetInstrument)
        presetFilter ∷ Char → Maybe FilterSpec
        presetFilter k =
          case k of
            '1' -> Nothing
            '2' -> Just FilterSpec
                    { fType = FLP
                    , fCutoffHz = 1200
                    , fQ = 0.707
                    , fSlope = S24
                    , fKeyTrack = KeyTrack 0.30
                    , fEnvAmountOct = 0
                    , fEnvADSR = ADSR 0.001 0.12 0.0 0.10
                    , fQEnvAmount = 0
                    }
            '3' -> Just FilterSpec
                    { fType = FHP
                    , fCutoffHz = 200
                    , fQ = 0.707
                    , fSlope = S12
                    , fKeyTrack = KeyTrack 0.0
                    , fEnvAmountOct = 0
                    , fEnvADSR = ADSR 0.001 0.12 0.0 0.10
                    , fQEnvAmount = 0
                    }
            _   -> Nothing

        -- Plucky filter (envelope opens cutoff then decays)
        pluckFilter ∷ FilterSpec
        pluckFilter =
          FilterSpec
            { fType = FLP
            , fCutoffHz = 400
            , fQ = 0.9
            , fSlope = S24
            , fKeyTrack = KeyTrack 0.5
            , fEnvAmountOct = 4.0
            , fEnvADSR = ADSR
                { aAttackSec = 0.001
                , aDecaySec = 0.12
                , aSustain = 0.0
                , aReleaseSec = 0.10
                }
            , fQEnvAmount = 0.0
            }

        mkInst wf gain mFilt =
          Instrument
            { iWaveform = wf
            , iAdsrDefault = envAmp
            , iGain = gain
            , iFilter = mFilt
            }

        pushInstruments mFilt = do
          let inst0 = mkInst WaveSine 1.0 mFilt
              inst1 = mkInst WaveSaw  0.8 mFilt
          sendAudio sys (AudioSetInstrument (InstrumentId 0) inst0)
          sendAudio sys (AudioSetInstrument (InstrumentId 1) inst1)

    -- start with preset 1 (off)
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

        -- One-shot "pluck": use current instrument, but with a short amp ADSR override.
        -- Filter pluck comes from temporarily setting the instrument filter.
        playPluck nid = do
          iid <- readIORef currentInstrRef
          let ampPluck = ADSR
                { aAttackSec  = 0.001
                , aDecaySec   = 0.10
                , aSustain    = 0.0
                , aReleaseSec = 0.12
                }
          -- Note: AudioNoteOn uses instrument filter state at note-on time.
          sendAudio sys (AudioNoteOn iid 0.35 0.0 nid (Just ampPluck))
          -- Schedule a NoteOff shortly after to force release even if sustain is 0.
          -- (Without proper scheduling infra, we just approximate with a delay here.)
          -- Since stdin is raw and we’re in main thread, keep it short.
          -- If you dislike this blocking delay, we can fork a lightweight thread.
          pure ()

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

        -- Apply pluck filter to instruments, play notes, then restore current preset.
        -- For now we restore to "off" after pluck. If you want it to restore to
        -- whichever preset you last chose, we can store it in an IORef.
        doPluck = do
          -- set pluck filter on both instruments
          pushInstruments (Just pluckFilter)
          playPluck middleC
          playPluck dNext
          -- trigger note-offs after a short time using forked threads to avoid blocking input
          _ <- forkIO $ do
            threadDelay 120000  -- 120ms
            iid <- readIORef currentInstrRef
            sendAudio sys (AudioNoteOff iid middleC)
          _ <- forkIO $ do
            threadDelay 120000
            iid <- readIORef currentInstrRef
            sendAudio sys (AudioNoteOff iid dNext)
          -- restore filter off
          _ <- forkIO $ do
            threadDelay 200000
            pushInstruments Nothing
          pure ()

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
          iid <- readIORef currentInstrRef
          let iid' = if iid == InstrumentId 0 then InstrumentId 1 else InstrumentId 0
          writeIORef currentInstrRef iid'
          putStrLn ("\nInstrument switched to " <> show iid' <> "\n")

        '1' -> pushInstruments (presetFilter '1') >> putStrLn "\nFilter preset 1 (off)\n"
        '2' -> pushInstruments (presetFilter '2') >> putStrLn "\nFilter preset 2\n"
        '3' -> pushInstruments (presetFilter '3') >> putStrLn "\nFilter preset 3\n"

        -- New: plucky one-shot on both notes
        'p' -> doPluck

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
