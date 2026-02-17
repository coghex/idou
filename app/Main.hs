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
    putStrLn "  c / d    : NoteOn C / D"
    putStrLn "  space    : NoteOffInstrument (panic / release all voices for inst 0)"
    putStrLn "  <tab>    : cycle waveform (live)"
    putStrLn "  u        : toggle unison (2 layers)"
    putStrLn "  s        : toggle hard-sync (layer1 -> layer0)"
    putStrLn "  m        : toggle mod-matrix routes"
    putStrLn "  l        : toggle play mode (MonoLegato <-> Poly)"
    putStrLn "  1 / 2 / 3: filter preset"
    putStrLn "  g        : toggle glide"
    putStrLn "  r        : toggle legato filter env retrigger"
    putStrLn "  a        : toggle legato amp env retrigger"
    putStrLn "  v        : toggle vibrato (also sets LFO1 rate for mod matrix)"
    putStrLn "  [ / ]    : vibrato depth -/+ (cents)"
    putStrLn "  - / =    : vibrato rate  -/+ (Hz)"
    putStrLn "  p        : pluck one-shot"
    putStrLn "  q        : quit"

    let envAmpNormal = ADSR 0.02 0.10 0.6 0.50
        envAmpPluck  = ADSR 0.001 0.10 0.0 0.12

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
            , fEnvADSR = ADSR 0.001 0.12 0.0 0.10
            , fQEnvAmount = 0.0
            }

        nextWaveform ∷ Waveform → Waveform
        nextWaveform wf =
          case wf of
            WaveSine     -> WaveSaw
            WaveSaw      -> WaveSquare
            WaveSquare   -> WaveTriangle
            WaveTriangle -> WaveSine

        iid0 = InstrumentId 0

        basePitch = PitchSpec 0 0 0 0
        detuneUp c = PitchSpec 0 0 c 0
        detuneDown c = PitchSpec 0 0 (-c) 0

        mkRoutes enabled =
          if not enabled
            then []
            else
              [ ModRoute ModSrcLfo1 ModDstFilterCutoffOct 0.5
              , ModRoute ModSrcLfo1 ModDstAmpGain (-0.3)
              , ModRoute ModSrcEnvAmp (ModDstLayerPitchCents 0) (-8)
              ]

        mkInst
          ∷ Waveform → Float → ADSR → Maybe FilterSpec
          → Bool → Bool → Bool
          → PlayMode
          → Instrument
        mkInst wf gain env mFilt unisonOn syncOn modsOn playMode =
          let layers
                | not unisonOn =
                    [ OscLayer wf basePitch 1.0 NoSync ]
                | otherwise =
                    [ OscLayer wf (detuneDown 7) 0.5 NoSync
                    , OscLayer wf (detuneUp 7)   0.5 (if syncOn then HardSyncTo 0 else NoSync)
                    ]
          in Instrument
              { iOscs = layers
              , iLayerSpread = 0.7
              , iAdsrDefault = env
              , iGain = gain
              , iFilter = mFilt
              , iModRoutes = mkRoutes modsOn

              -- NEW poly config
              , iPlayMode = playMode
              , iPolyMax = 8
              , iVoiceSteal = StealQuietest
              }

    wfRef       <- newIORef WaveSaw
    filtRef     <- newIORef (Nothing ∷ Maybe FilterSpec)
    unisonRef   <- newIORef False
    syncRef     <- newIORef False
    modsRef     <- newIORef True
    playModeRef <- newIORef Poly
    instGainRef <- newIORef 1.0

    let pushInst0 = do
          wf <- readIORef wfRef
          mf <- readIORef filtRef
          uni <- readIORef unisonRef
          sy <- readIORef syncRef
          mo <- readIORef modsRef
          pm <- readIORef playModeRef
          g0 <- readIORef instGainRef
          let inst0 = mkInst wf g0 envAmpNormal mf uni sy mo pm
          sendAudio sys (AudioSetInstrument iid0 inst0)

    pushInst0

    glideOnRef  <- newIORef False
    fRetrigRef  <- newIORef False
    aRetrigRef  <- newIORef False

    vibOnRef    <- newIORef False
    vibRateRef  <- newIORef 5.0
    vibDepthRef <- newIORef 20.0

    let middleC = NoteMidi 60
        dNext   = NoteMidi 62

        noteOn nid =
          sendAudio sys (AudioNoteOn iid0 0.25 0.0 nid Nothing)

        noteOffAll =
          sendAudio sys (AudioNoteOffInstrument iid0)

        doPluck = do
          writeIORef filtRef (Just pluckFilter)
          pushInst0
          sendAudio sys (AudioNoteOn iid0 0.35 0.0 middleC (Just envAmpPluck))
          _ <- forkIO $ do
            threadDelay 120000
            sendAudio sys (AudioNoteOffInstrument iid0)
          _ <- forkIO $ do
            threadDelay 200000
            writeIORef filtRef Nothing
            pushInst0
          pure ()

        cycleWaveformsLive = modifyIORef' wfRef nextWaveform >> pushInst0

        toggleUnison = modifyIORef' unisonRef not >> pushInst0
        toggleSync   = modifyIORef' syncRef not >> pushInst0

        toggleMods = do
          modifyIORef' modsRef not
          pushInst0
          on <- readIORef modsRef
          putStrLn $ if on then "\nMod matrix: ON\n" else "\nMod matrix: OFF\n"

        togglePlayMode = do
          modifyIORef' playModeRef (\pm -> case pm of MonoLegato -> Poly; Poly -> MonoLegato)
          pushInst0
          pm <- readIORef playModeRef
          putStrLn $ "\nPlayMode: " <> show pm <> "\n"

        setFilterPreset k = writeIORef filtRef (presetFilter k) >> pushInst0

        toggleGlide = do
          on <- readIORef glideOnRef
          let on' = not on
              glideSec = if on' then 0.12 else 0.0
          writeIORef glideOnRef on'
          sendAudio sys (AudioSetGlideSec iid0 glideSec)

        toggleFilterRetrig = do
          on <- readIORef fRetrigRef
          let on' = not on
          writeIORef fRetrigRef on'
          sendAudio sys (AudioSetLegatoFilterRetrig iid0 on')

        toggleAmpRetrig = do
          on <- readIORef aRetrigRef
          let on' = not on
          writeIORef aRetrigRef on'
          sendAudio sys (AudioSetLegatoAmpRetrig iid0 on')

        applyVibrato0 = do
          on <- readIORef vibOnRef
          rate <- readIORef vibRateRef
          depth <- readIORef vibDepthRef
          let (r,d) = if on then (rate, depth) else (0, 0)
          sendAudio sys (AudioSetVibrato iid0 r d)

        toggleVibrato = modifyIORef' vibOnRef not >> applyVibrato0
        vibDepthDelta d = modifyIORef' vibDepthRef (\x -> max 0 (x + d)) >> applyVibrato0
        vibRateDelta  d = modifyIORef' vibRateRef  (\x -> max 0 (x + d)) >> applyVibrato0

    applyVibrato0

    forever $ do
      ch <- hGetChar stdin
      case ch of
        'q' -> noteOffAll >> putStrLn "\nQuit." >> exitSuccess

        '\t' -> cycleWaveformsLive
        'u'  -> toggleUnison
        's'  -> toggleSync
        'm'  -> toggleMods
        'l'  -> togglePlayMode

        '1' -> setFilterPreset '1'
        '2' -> setFilterPreset '2'
        '3' -> setFilterPreset '3'

        'g' -> toggleGlide
        'r' -> toggleFilterRetrig
        'a' -> toggleAmpRetrig

        'v' -> toggleVibrato
        '[' -> vibDepthDelta (-5)
        ']' -> vibDepthDelta 5
        '-' -> vibRateDelta (-0.5)
        '=' -> vibRateDelta 0.5

        'p' -> doPluck

        'c' -> noteOn middleC
        'd' -> noteOn dNext
        ' ' -> noteOffAll

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
