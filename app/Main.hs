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
    putStrLn "Terminal synth test (mono legato mode):"
    putStrLn "  c / d    : set pitch to C / D (legato; will glide if enabled)"
    putStrLn "  space    : release (NoteOffInstrument)"
    putStrLn "  <tab>    : LIVE cycle waveform on instruments 0 and 1 (sine->saw->square->tri)"
    putStrLn "  1 / 2 / 3: select filter preset (updates instruments 0 and 1)"
    putStrLn "  g        : toggle glide (portamento) on instruments 0 and 1"
    putStrLn "  r        : toggle legato filter env retrigger on instruments 0 and 1"
    putStrLn "  a        : toggle legato amp env retrigger on instruments 0 and 1"
    putStrLn "  v        : toggle vibrato on instrument 0 (pitch LFO)"
    putStrLn "  [ / ]    : vibrato depth -/+ (cents)"
    putStrLn "  - / =    : vibrato rate  -/+ (Hz)"
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

        iid0 = InstrumentId 0
        iid1 = InstrumentId 1

    -- Mutable instrument definitions
    inst0Ref <- newIORef (mkInst WaveSine 1.0 envAmpNormal Nothing)
    inst1Ref <- newIORef (mkInst WaveSaw  0.8 envAmpNormal Nothing)

    let push0 = readIORef inst0Ref >>= sendAudio sys . AudioSetInstrument iid0
        push1 = readIORef inst1Ref >>= sendAudio sys . AudioSetInstrument iid1
        pushBoth = push0 >> push1

        setFilters mFilt = do
          modifyIORef' inst0Ref (\i -> i { iFilter = mFilt })
          modifyIORef' inst1Ref (\i -> i { iFilter = mFilt })
          pushBoth

    pushBoth

    glideOnRef  <- newIORef False
    fRetrigRef  <- newIORef False
    aRetrigRef  <- newIORef False

    vibOnRef    <- newIORef False
    vibRateRef  <- newIORef 5.0   -- Hz
    vibDepthRef <- newIORef 20.0  -- cents

    let middleC = NoteMidi 60
        dNext   = NoteMidi 62

        noteOnLegato nid =
          sendAudio sys (AudioNoteOn iid0 0.25 0.0 nid Nothing)

        noteOffLegato =
          sendAudio sys (AudioNoteOffInstrument iid0)

        playPluck iid nid =
          sendAudio sys (AudioNoteOn iid 0.35 0.0 nid (Just envAmpPluck))

        doPluck = do
          setFilters (Just pluckFilter)
          playPluck iid0 middleC
          playPluck iid1 dNext

          _ <- forkIO $ do
            threadDelay 120000
            sendAudio sys (AudioNoteOffInstrument iid0)
          _ <- forkIO $ do
            threadDelay 120000
            sendAudio sys (AudioNoteOffInstrument iid1)
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

        toggleGlide = do
          on <- readIORef glideOnRef
          let on' = not on
              glideSec = if on' then 0.12 else 0.0
          writeIORef glideOnRef on'
          sendAudio sys (AudioSetGlideSec iid0 glideSec)
          sendAudio sys (AudioSetGlideSec iid1 glideSec)
          putStrLn $ if on' then "\nGlide ON (0.12s)\n" else "\nGlide OFF\n"

        toggleFilterRetrig = do
          on <- readIORef fRetrigRef
          let on' = not on
          writeIORef fRetrigRef on'
          sendAudio sys (AudioSetLegatoFilterRetrig iid0 on')
          sendAudio sys (AudioSetLegatoFilterRetrig iid1 on')
          putStrLn $
            if on'
              then "\nLegato filter env retrigger: ON\n"
              else "\nLegato filter env retrigger: OFF\n"

        toggleAmpRetrig = do
          on <- readIORef aRetrigRef
          let on' = not on
          writeIORef aRetrigRef on'
          sendAudio sys (AudioSetLegatoAmpRetrig iid0 on')
          sendAudio sys (AudioSetLegatoAmpRetrig iid1 on')
          putStrLn $
            if on'
              then "\nLegato amp env retrigger: ON\n"
              else "\nLegato amp env retrigger: OFF\n"

        applyVibrato0 = do
          on <- readIORef vibOnRef
          rate <- readIORef vibRateRef
          depth <- readIORef vibDepthRef
          let (r,d) = if on then (rate, depth) else (0, 0)
          sendAudio sys (AudioSetVibrato iid0 r d)

        printVib = do
          on <- readIORef vibOnRef
          rate <- readIORef vibRateRef
          depth <- readIORef vibDepthRef
          putStrLn $
            "\nVibrato " <> (if on then "ON" else "OFF")
              <> "  rate=" <> show rate <> "Hz"
              <> "  depth=" <> show depth <> "c\n"

        toggleVibrato = do
          modifyIORef' vibOnRef not
          applyVibrato0
          printVib

        vibDepthDelta d = do
          modifyIORef' vibDepthRef (\x -> max 0 (x + d))
          applyVibrato0
          printVib

        vibRateDelta d = do
          modifyIORef' vibRateRef (\x -> max 0 (x + d))
          applyVibrato0
          printVib

    -- default vibrato off
    applyVibrato0

    forever $ do
      ch <- hGetChar stdin
      case ch of
        'q' -> do
          noteOffLegato
          putStrLn "\nQuit."
          exitSuccess

        '\t' -> cycleWaveformsLive

        '1' -> setFilters (presetFilter '1') >> putStrLn "\nFilter preset 1 (off)\n"
        '2' -> setFilters (presetFilter '2') >> putStrLn "\nFilter preset 2\n"
        '3' -> setFilters (presetFilter '3') >> putStrLn "\nFilter preset 3\n"

        'g' -> toggleGlide
        'r' -> toggleFilterRetrig
        'a' -> toggleAmpRetrig

        'v' -> toggleVibrato
        '[' -> vibDepthDelta (-5)
        ']' -> vibDepthDelta 5
        '-' -> vibRateDelta (-0.5)
        '=' -> vibRateDelta 0.5

        'p' -> doPluck

        'c' -> noteOnLegato middleC
        'd' -> noteOnLegato dNext
        ' ' -> noteOffLegato

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
