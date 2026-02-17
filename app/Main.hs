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
    putStrLn "  <tab>    : LIVE cycle waveform on instruments 0 and 1 (sine->saw->square->tri)"
    putStrLn "  1 / 2 / 3: select filter preset (updates instruments 0 and 1)"
    putStrLn "  p        : play plucky sound (one-shot) on both notes"
    putStrLn "  q        : quit"

    let envAmpNormal = ADSR
          { aAttackSec  = 0.02
          , aDecaySec   = 0.10
          , aSustain    = 0.6
          , aReleaseSec = 0.50
          }

        envAmpPluck = ADSR
          { aAttackSec  = 0.001
          , aDecaySec   = 0.10
          , aSustain    = 0.0
          , aReleaseSec = 0.12
          }

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

        mkInst wf gain env mFilt =
          Instrument
            { iWaveform = wf
            , iAdsrDefault = env
            , iGain = gain
            , iFilter = mFilt
            }

        nextWaveform ∷ Waveform → Waveform
        nextWaveform wf =
          case wf of
            WaveSine     -> WaveSaw
            WaveSaw      -> WaveSquare
            WaveSquare   -> WaveTriangle
            WaveTriangle -> WaveSine

    -- Keep mutable instrument definitions so we can update them live and resend.
    inst0Ref <- newIORef (mkInst WaveSine 1.0 envAmpNormal Nothing)
    inst1Ref <- newIORef (mkInst WaveSaw  0.8 envAmpNormal Nothing)

    let push0 = readIORef inst0Ref >>= sendAudio sys . AudioSetInstrument (InstrumentId 0)
        push1 = readIORef inst1Ref >>= sendAudio sys . AudioSetInstrument (InstrumentId 1)
        pushBoth = push0 >> push1

        setFilters mFilt = do
          modifyIORef' inst0Ref (\i -> i { iFilter = mFilt })
          modifyIORef' inst1Ref (\i -> i { iFilter = mFilt })
          pushBoth

    -- initial instrument setup
    pushBoth

    -- Track which instrument started each note so NoteOff always matches.
    cHeldRef <- newIORef (Nothing ∷ Maybe InstrumentId)
    dHeldRef <- newIORef (Nothing ∷ Maybe InstrumentId)

    let middleC = NoteMidi 60
        dNext   = NoteMidi 62

        noteOn iid nid =
          sendAudio sys (AudioNoteOn iid 0.25 0.0 nid Nothing)

        noteOff iid nid =
          sendAudio sys (AudioNoteOff iid nid)

        playPluck iid nid =
          sendAudio sys (AudioNoteOn iid 0.35 0.0 nid (Just envAmpPluck))

        toggleNote heldRef iid nid = do
          held <- readIORef heldRef
          case held of
            Nothing -> do
              noteOn iid nid
              writeIORef heldRef (Just iid)
            Just _ -> do
              noteOff iid nid
              writeIORef heldRef Nothing

        doPluck = do
          setFilters (Just pluckFilter)
          playPluck (InstrumentId 0) middleC
          playPluck (InstrumentId 1) dNext

          _ <- forkIO $ do
            threadDelay 120000
            sendAudio sys (AudioNoteOff (InstrumentId 0) middleC)
          _ <- forkIO $ do
            threadDelay 120000
            sendAudio sys (AudioNoteOff (InstrumentId 1) dNext)
          _ <- forkIO $ do
            threadDelay 200000
            setFilters Nothing
          pure ()

        cycleWaveformsLive = do
          modifyIORef' inst0Ref (\i -> i { iWaveform = nextWaveform (iWaveform i) })
          modifyIORef' inst1Ref (\i -> i { iWaveform = nextWaveform (iWaveform i) })
          pushBoth
          i0 <- readIORef inst0Ref
          putStrLn ("\nWaveform -> " <> show (iWaveform i0) <> " (applied live)\n")

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

        '\t' ->
          -- LIVE cycle across all 4 waveforms on both instruments;
          -- since your audio thread applies AudioSetInstrument to active voices,
          -- held notes will change timbre immediately.
          cycleWaveformsLive

        '1' -> setFilters (presetFilter '1') >> putStrLn "\nFilter preset 1 (off)\n"
        '2' -> setFilters (presetFilter '2') >> putStrLn "\nFilter preset 2\n"
        '3' -> setFilters (presetFilter '3') >> putStrLn "\nFilter preset 3\n"

        'p' -> doPluck

        -- Keep your original behavior: c uses instrument 0, d uses instrument 1.
        -- That makes it easy to compare waveforms live.
        'c' -> toggleNote cHeldRef (InstrumentId 0) middleC
        'd' -> toggleNote dHeldRef (InstrumentId 1) dNext

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
