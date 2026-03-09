{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (forM, unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.List (find, intercalate, isInfixOf)
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Foreign.ForeignPtr (mallocForeignPtrArray)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.C (CFloat)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (openBinaryTempFile, hClose)

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU

import Audio.Config (AudioBufferConfig(..), AudioConfig(..), AudioTelemetryConfig(..), parseAudioConfigText)
import Audio.Envelope
import Audio.Oscillator (Osc(..), oscInitSeeded, oscStep)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument, gmDrumInstrument, gmProgramInstrument, gmPercussionChannel)
import Audio.Thread.Render (renderChunkFrames)
import Audio.Thread.InstrumentTable
  ( MidiControls(..)
  , lookupMidiControls
  , resetMidiControls
  , setChannelAftertouch
  , setChannelPan
  , setChannelVolume
  , setExpression
  , setInstrument
  , setModWheel
  , setPitchBend
  )
import Audio.Thread.Types (AudioState(..), ClipAsset(..), ClipSource(..), ScheduledItem(..), Voice(..))
import Audio.Thread.Voice
  ( addInstrumentNote
  , applyInstrumentToActiveVoices
  , panGains
  , releaseInstrumentAllVoices
  , releaseInstrumentNote
  , setInstrumentNoteAftertouch
  )
import Audio.Types
import Midi.Control (controllerPanValue, controllerValue01, midiPitchBendSemitones, sustainPedalDown)
import Midi.Play.Util
  ( pushHeldNote
  , popHeldNote
  , enqueueDeferredRelease
  , takeDeferredReleases
  , ticksToMicroseconds
  )
import Engine.Core.Queue (Queue, newQueue, tryReadQueue)
import Sound.Miniaudio.Decode (DecodedAudio(..), decodeAudioFileStereoF32)
import Sound.Miniaudio.RingBuffer (rbCreateF32, rbDestroy, rbReadF32)

data TestCase = TestCase
  { tcName   ∷ String
  , tcAction ∷ IO ()
  }

main ∷ IO ()
main = do
  filters <- getArgs
  let selected = filter (matchesAny filters . tcName) testCases
  when (null selected) $ do
    putStrLn ("No tests matched filters: " <> unwords filters)
    putStrLn ("Available tests: " <> intercalate ", " (map tcName testCases))
    exitFailure

  results <- forM selected runTest
  let failures = [name | (name, False) <- results]
  unless (null failures) $ do
    putStrLn ("Failed tests: " <> intercalate ", " failures)
    exitFailure

matchesAny ∷ [String] → String → Bool
matchesAny [] _ = True
matchesAny needles haystack = any (`isInfixOf` haystack) needles

runTest ∷ TestCase → IO (String, Bool)
runTest tc = do
  putStrLn ("[test] " <> tcName tc)
  result <- try (tcAction tc) ∷ IO (Either SomeException ())
  case result of
    Left ex -> do
      putStrLn ("[fail] " <> tcName tc <> ": " <> show ex)
      pure (tcName tc, False)
    Right () -> do
      putStrLn ("[pass] " <> tcName tc)
      pure (tcName tc, True)

testCases ∷ [TestCase]
testCases =
  [ TestCase "audio-config-parses-yaml-shape" testAudioConfigParsesYamlShape
  , TestCase "audio-config-rejects-too-small-buffer" testAudioConfigRejectsTooSmallBuffer
  , TestCase "envelope-release-reaches-done" testEnvelopeReleaseReachesDone
  , TestCase "mono-adsr-override-on-new-voice" testMonoAdsrOverrideOnNewVoice
  , TestCase "mono-legato-reuses-voice-and-updates-override" testMonoLegatoReusesVoiceAndUpdatesOverride
  , TestCase "mono-legato-sustain-style-stale-release-is-ignored" testMonoLegatoSustainStyleStaleReleaseIsIgnored
  , TestCase "mono-legato-sustain-style-current-release-still-works" testMonoLegatoSustainStyleCurrentReleaseStillWorks
  , TestCase "release-targets-matching-note-instance" testReleaseTargetsMatchingNoteInstance
  , TestCase "poly-voice-steal-reuses-released-slot" testPolyVoiceStealReusesReleasedSlot
  , TestCase "release-all-voices-keeps-other-instruments-active" testReleaseAllVoicesKeepsOtherInstrumentsActive
  , TestCase "midi-controller-state-clamps-and-resets" testMidiControllerStateClampsAndResets
  , TestCase "midi-controller-conversions-are-normalized" testMidiControllerConversionsAreNormalized
  , TestCase "midi-held-notes-release-oldest-instance-first" testMidiHeldNotesReleaseOldestInstanceFirst
  , TestCase "midi-deferred-releases-preserve-noteoff-order" testMidiDeferredReleasesPreserveNoteOffOrder
  , TestCase "midi-tick-conversion-basic" testMidiTickConversionBasic
  , TestCase "midi-tick-conversion-rejects-zero-ppqn" testMidiTickConversionRejectsZeroPpqn
  , TestCase "poly-aftertouch-targets-matching-note-instance" testPolyAftertouchTargetsMatchingNoteInstance
  , TestCase "aftertouch-routes-are-wired-into-expressive-patches" testAftertouchRoutesAreWiredIntoExpressivePatches
  , TestCase "noise-waveforms-mix-between-white-and-pink" testNoiseWaveformsMixBetweenWhiteAndPink
  , TestCase "pink-noise-is-smoother-than-white" testPinkNoiseIsSmootherThanWhite
  , TestCase "noise-layer-envelope-shapes-amplitude" testNoiseLayerEnvelopeShapesAmplitude
  , TestCase "gm-drum-noise-layers-use-envelopes" testGmDrumNoiseLayersUseEnvelopes
  , TestCase "gm-program-map-varies-by-family" testGmProgramMapVariesByFamily
  , TestCase "gm-percussion-channel-ignores-program" testGmPercussionChannelIgnoresProgram
  , TestCase "gm-drum-map-varies-by-key-family" testGmDrumMapVariesByKeyFamily
  , TestCase "instrument-load-keeps-active-voice-until-live-apply" testInstrumentLoadKeepsActiveVoiceUntilLiveApply
  , TestCase "clip-one-shot-auto-releases-source" testClipOneShotAutoReleasesSource
  , TestCase "clip-music-bus-gain-zero-mutes-output" testClipMusicBusGainZeroMutesOutput
  , TestCase "clip-finished-emits-audio-event" testClipFinishedEmitsAudioEvent
  , TestCase "note-finished-emits-audio-event" testNoteFinishedEmitsAudioEvent
  , TestCase "scheduled-bus-gain-switches-at-frame" testScheduledBusGainSwitchesAtFrame
  , TestCase "scheduled-note-on-off-trigger-at-frames" testScheduledNoteOnOffTriggerAtFrames
  , TestCase "wav-decoder-loads-stereo-f32" testWavDecoderLoadsStereoF32
  ]

testAudioConfigParsesYamlShape ∷ IO ()
testAudioConfigParsesYamlShape = do
  let yamlText =
        unlines
          [ "audio:"
          , "  sample_rate: 44100"
          , "  chunk_frames: 128"
          , "  max_voices: 64"
          , "  buffer:"
          , "    capacity_frames: 2048"
          , "    target_chunks: 8"
          ]
      expected =
        AudioConfig
          { acSampleRate = 44100
          , acChunkFrames = 128
          , acMaxVoices = 64
          , acBuffer =
              AudioBufferConfig
                { abCapacityFrames = 2048
                , abTargetChunks = 8
                }
          , acTelemetry =
              AudioTelemetryConfig
                { atVerboseReportEveryLoops = 1000
                , atPartialWriteAlertThreshold = 1
                }
          }
  assertEqual "parsed audio config" (Right expected) (parseAudioConfigText yamlText)

testAudioConfigRejectsTooSmallBuffer ∷ IO ()
testAudioConfigRejectsTooSmallBuffer = do
  let yamlText =
        unlines
          [ "audio:"
          , "  sample_rate: 48000"
          , "  chunk_frames: 512"
          , "  max_voices: 256"
          , "  buffer:"
          , "    capacity_frames: 1024"
          , "    target_chunks: 4"
          ]
  case parseAudioConfigText yamlText of
    Left err ->
      assertBool "expected capacity validation error" ("capacity_frames" `isInfixOf` err)
    Right cfg ->
      error ("expected config parse failure, got " <> show cfg)

testEnvelopeReleaseReachesDone ∷ IO ()
testEnvelopeReleaseReachesDone = do
  let adsr = ADSR 0.01 0.02 0.4 0.01
      released0 = envRelease (EnvState EnvSustain 0.8)
      finalState = stepEnvN 1024 adsr released0
  assertEqual "envelope stage" EnvDone (eStage finalState)
  assertEqual "envelope level" 0 (eLevel finalState)

testMonoAdsrOverrideOnNewVoice ∷ IO ()
testMonoAdsrOverrideOnNewVoice = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument MonoLegato
      override = ADSR 0.005 0.01 1.5 0.02
      expected = override { aSustain = 1 }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) (NoteInstanceId 1) 0.8 (Just override)
  assertEqual "active voice count" 1 (stActiveCount st2)
  voice <- MV.read (stVoices st2) 0
  assertEqual "voice ADSR" expected (vADSR voice)
  assertEqual "voice instance" (Just (NoteInstanceId 1)) (vNoteInstanceId voice)

testMonoLegatoReusesVoiceAndUpdatesOverride ∷ IO ()
testMonoLegatoReusesVoiceAndUpdatesOverride = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument MonoLegato
      override1 = ADSR 0.01 0.02 0.6 0.03
      override2 = ADSR 0.02 0.04 (-0.5) 0.05
      expected2 = override2 { aSustain = 0 }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) (NoteInstanceId 1) 0.7 (Just override1)
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 67) (NoteInstanceId 2) 0.7 (Just override2)
  assertEqual "active voice count after legato reuse" 1 (stActiveCount st3)
  assertEqual "monotonic voice timestamp" 2 (stNow st3)
  voice <- MV.read (stVoices st3) 0
  assertEqual "reused voice note key" (Just (NoteKey 67)) (vNoteKey voice)
  assertEqual "reused voice instance" (Just (NoteInstanceId 2)) (vNoteInstanceId voice)
  assertEqual "reused voice ADSR" expected2 (vADSR voice)

testMonoLegatoSustainStyleStaleReleaseIsIgnored ∷ IO ()
testMonoLegatoSustainStyleStaleReleaseIsIgnored = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument MonoLegato
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 67) secondId 0.8 Nothing
  st4 <- releaseInstrumentNote iid firstId st3
  assertEqual "active voice count after stale release" 1 (stActiveCount st4)
  voice <- MV.read (stVoices st4) 0
  assertEqual "voice should still be newest key" (Just (NoteKey 67)) (vNoteKey voice)
  assertEqual "voice should still be newest instance" (Just secondId) (vNoteInstanceId voice)
  assertBool "stale release should not release current legato voice" (eStage (vEnv voice) /= EnvRelease)

testMonoLegatoSustainStyleCurrentReleaseStillWorks ∷ IO ()
testMonoLegatoSustainStyleCurrentReleaseStillWorks = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument MonoLegato
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 67) secondId 0.8 Nothing
  st4 <- releaseInstrumentNote iid firstId st3
  st5 <- releaseInstrumentNote iid secondId st4
  voice <- MV.read (stVoices st5) 0
  assertEqual "current instance should enter release" EnvRelease (eStage (vEnv voice))

testReleaseTargetsMatchingNoteInstance ∷ IO ()
testReleaseTargetsMatchingNoteInstance = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument Poly
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 64) secondId 0.8 Nothing
  st4 <- releaseInstrumentNote iid firstId st3
  voices <- readActiveVoices st4
  released <- findVoice firstId voices
  unreleased <- findVoice secondId voices
  assertEqual "released voice stage" EnvRelease (eStage (vEnv released))
  assertBool "non-target voice should remain active" (eStage (vEnv unreleased) /= EnvRelease)

testPolyVoiceStealReusesReleasedSlot ∷ IO ()
testPolyVoiceStealReusesReleasedSlot = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = (mkInstrument Poly) { iPolyMax = 2 }
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
      thirdId = NoteInstanceId 3
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 64) secondId 0.8 Nothing
  st4 <- releaseInstrumentNote iid firstId st3
  st5 <- addInstrumentNote handle st4 iid 1 0 (NoteKey 67) thirdId 0.8 Nothing
  voices <- readActiveVoices st5
  assertEqual "active voice count after steal" 2 (stActiveCount st5)
  assertBool "released voice should be replaced" (all ((/= Just firstId) . vNoteInstanceId) voices)
  assertBool "second voice should survive steal" (any ((== Just secondId) . vNoteInstanceId) voices)
  assertBool "new voice should be present" (any ((== Just thirdId) . vNoteInstanceId) voices)

testReleaseAllVoicesKeepsOtherInstrumentsActive ∷ IO ()
testReleaseAllVoicesKeepsOtherInstrumentsActive = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid0 = InstrumentId 0
      iid1 = InstrumentId 1
      inst = mkInstrument Poly
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
      thirdId = NoteInstanceId 3
  st1 <- setInstrument iid0 inst st0
  st2 <- setInstrument iid1 inst st1
  st3 <- addInstrumentNote handle st2 iid0 1 0 (NoteKey 60) firstId 0.8 Nothing
  st4 <- addInstrumentNote handle st3 iid0 1 0 (NoteKey 64) secondId 0.8 Nothing
  st5 <- addInstrumentNote handle st4 iid1 1 0 (NoteKey 67) thirdId 0.8 Nothing
  st6 <- releaseInstrumentAllVoices iid0 st5
  voices <- readActiveVoices st6
  firstVoice <- findVoice firstId voices
  secondVoice <- findVoice secondId voices
  thirdVoice <- findVoice thirdId voices
  assertEqual "first released voice stage" EnvRelease (eStage (vEnv firstVoice))
  assertEqual "second released voice stage" EnvRelease (eStage (vEnv secondVoice))
  assertEqual "first released voice hold" 0 (vHoldRemain firstVoice)
  assertEqual "second released voice hold" 0 (vHoldRemain secondVoice)
  assertBool "other instrument should remain active" (eStage (vEnv thirdVoice) /= EnvRelease)
  assertBool "other instrument hold should remain" (vHoldRemain thirdVoice > 0)

testMidiControllerStateClampsAndResets ∷ IO ()
testMidiControllerStateClampsAndResets = do
  st0 <- mkTestState
  let iid = InstrumentId 0
  st1 <- setChannelVolume iid 1.5 st0
  st2 <- setExpression iid (-0.25) st1
  st3 <- setChannelPan iid 2 st2
  st4 <- setModWheel iid 0.4 st3
  st5 <- setChannelAftertouch iid 1.5 st4
  st6 <- setPitchBend iid (defaultPitchBendRangeSemitones * 2) st5
  ctrls <- lookupMidiControls iid st6
  assertEqual
    "clamped controller state"
    MidiControls
      { mcChannelVolume = 1
      , mcExpression = 0
      , mcChannelPan = 1
      , mcModWheel = 0.4
      , mcChannelAftertouch = 1
      , mcPitchBendSemis = defaultPitchBendRangeSemitones
      }
    ctrls
  st7 <- resetMidiControls iid st6
  resetCtrls <- lookupMidiControls iid st7
  assertEqual
    "reset controller state"
    MidiControls
      { mcChannelVolume = 1
      , mcExpression = 1
      , mcChannelPan = 0
      , mcModWheel = 0
      , mcChannelAftertouch = 0
      , mcPitchBendSemis = 0
      }
    resetCtrls

testMidiControllerConversionsAreNormalized ∷ IO ()
testMidiControllerConversionsAreNormalized = do
  assertNear "controller min" 0 (controllerValue01 0)
  assertNear "controller max" 1 (controllerValue01 127)
  assertNear "controller pan left" (-1) (controllerPanValue 0)
  assertNear "controller pan center" 0 (controllerPanValue 64)
  assertNear "controller pan right" 1 (controllerPanValue 127)
  assertBool "sustain below threshold" (not (sustainPedalDown 63))
  assertBool "sustain at threshold" (sustainPedalDown 64)
  assertNear "pitch bend min" (negate defaultPitchBendRangeSemitones) (midiPitchBendSemitones 0)
  assertNear "pitch bend center" 0 (midiPitchBendSemitones 8192)
  assertNear "pitch bend max" defaultPitchBendRangeSemitones (midiPitchBendSemitones 16383)

testMidiHeldNotesReleaseOldestInstanceFirst ∷ IO ()
testMidiHeldNotesReleaseOldestInstanceFirst = do
  let firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
      held0 = M.empty
      held1 = pushHeldNote 0 60 firstId held0
      held2 = pushHeldNote 0 60 secondId held1
      (released1, held3) = popHeldNote 0 60 held2
      (released2, held4) = popHeldNote 0 60 held3
  assertEqual "first release should match first note-on" (Just firstId) released1
  assertEqual "second release should match second note-on" (Just secondId) released2
  assertBool "held stack should be empty after two pops" (M.null held4)

testMidiDeferredReleasesPreserveNoteOffOrder ∷ IO ()
testMidiDeferredReleasesPreserveNoteOffOrder = do
  let firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
      held0 = M.empty
      held1 = pushHeldNote 0 60 firstId held0
      held2 = pushHeldNote 0 60 secondId held1
      (released1, held3) = popHeldNote 0 60 held2
      deferred1 =
        case released1 of
          Nothing -> M.empty
          Just instId -> enqueueDeferredRelease 0 instId M.empty
      (released2, _held4) = popHeldNote 0 60 held3
      deferred2 =
        case released2 of
          Nothing -> deferred1
          Just instId -> enqueueDeferredRelease 0 instId deferred1
      (flushedIds, deferred3) = takeDeferredReleases 0 deferred2
  assertEqual "deferred flush order should follow note-off order" [firstId, secondId] flushedIds
  assertBool "channel deferred list should be removed after flush" (M.null deferred3)

testMidiTickConversionBasic ∷ IO ()
testMidiTickConversionBasic =
  assertEqual
    "tick conversion should match tempo math"
    250000
    (ticksToMicroseconds 480 500000 240)

testMidiTickConversionRejectsZeroPpqn ∷ IO ()
testMidiTickConversionRejectsZeroPpqn = do
  result <- try (evaluate (ticksToMicroseconds 0 500000 240)) ∷ IO (Either SomeException Int)
  case result of
    Left _ -> pure ()
    Right v -> error ("expected exception for ppqn=0, got " <> show v)

testPolyAftertouchTargetsMatchingNoteInstance ∷ IO ()
testPolyAftertouchTargetsMatchingNoteInstance = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst = mkInstrument Poly
      firstId = NoteInstanceId 1
      secondId = NoteInstanceId 2
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid 1 0 (NoteKey 64) secondId 0.8 Nothing
  st4 <- setInstrumentNoteAftertouch iid secondId 1.5 st3
  voices <- readActiveVoices st4
  firstVoice <- findVoice firstId voices
  secondVoice <- findVoice secondId voices
  assertNear "first voice aftertouch remains zero" 0 (vNoteAftertouch firstVoice)
  assertNear "second voice aftertouch is clamped" 1 (vNoteAftertouch secondVoice)

testAftertouchRoutesAreWiredIntoExpressivePatches ∷ IO ()
testAftertouchRoutesAreWiredIntoExpressivePatches = do
  let strings = gmProgramInstrument 48
      brass = gmProgramInstrument 56
      lead = gmProgramInstrument 80
      pad = gmProgramInstrument 88
  assertBool "strings patch should expose channel aftertouch" (hasModSrc ModSrcChanAftertouch (iModRoutes strings))
  assertBool "brass patch should expose channel aftertouch" (hasModSrc ModSrcChanAftertouch (iModRoutes brass))
  assertBool "lead patch should expose poly aftertouch" (hasModSrc ModSrcPolyAftertouch (iModRoutes lead))
  assertBool "pad patch should expose poly aftertouch" (hasModSrc ModSrcPolyAftertouch (iModRoutes pad))

testNoiseWaveformsMixBetweenWhiteAndPink ∷ IO ()
testNoiseWaveformsMixBetweenWhiteAndPink = do
  let blend = 0.35
      srF = fromIntegral testSampleRate
      white0 = oscInitSeeded WaveWhiteNoise Nothing srF 0 0x12345678
      pink0 = oscInitSeeded WavePinkNoise Nothing srF 0 0x12345678
      mix0 = oscInitSeeded (WaveNoiseMix blend) Nothing srF 0 0x12345678
      go ∷ Int → Osc → Osc → Osc → IO ()
      go 0 _ _ _ = pure ()
      go n ow op om = do
        let (ow1, whiteSample) = oscStep ow
            (op1, pinkSample) = oscStep op
            (om1, mixSample) = oscStep om
            expected = ((1 - blend) * whiteSample) + (blend * pinkSample)
        assertNear "mixed noise sample" expected mixSample
        go (n - 1) ow1 op1 om1
  go 256 white0 pink0 mix0

testPinkNoiseIsSmootherThanWhite ∷ IO ()
testPinkNoiseIsSmootherThanWhite = do
  let srF = fromIntegral testSampleRate
      white = takeOscSamples 2048 (oscInitSeeded WaveWhiteNoise Nothing srF 0 0x87654321)
      pink = takeOscSamples 2048 (oscInitSeeded WavePinkNoise Nothing srF 0 0x87654321)
      whiteDelta = meanAbsDelta white
      pinkDelta = meanAbsDelta pink
  assertBool "pink noise should vary more smoothly than white noise" (pinkDelta < whiteDelta * 0.75)

testNoiseLayerEnvelopeShapesAmplitude ∷ IO ()
testNoiseLayerEnvelopeShapesAmplitude = do
  let srF = fromIntegral testSampleRate
      env = ADSR 0 0.0015 0 0.001
      samples = takeOscSamples 256 (oscInitSeeded WaveWhiteNoise (Just env) srF 0 0xA5A5A5A5)
      early = meanAbs (take 32 samples)
      late = meanAbs (take 32 (drop 160 samples))
  assertBool "noise layer envelope should decay toward silence" (early > late * 8)

testGmDrumNoiseLayersUseEnvelopes ∷ IO ()
testGmDrumNoiseLayersUseEnvelopes = do
  let snare = gmDrumInstrument 38
      hat = gmDrumInstrument 42
      crash = gmDrumInstrument 49
      ride = gmDrumInstrument 51
  assertBool "snare should use white noise" (any (waveformIs (== WaveWhiteNoise) . olWaveform) (iOscs snare))
  assertBool "hihat should use blended noise" (any (waveformIs isNoiseMix . olWaveform) (iOscs hat))
  assertBool "crash should use pink noise" (any (waveformIs (== WavePinkNoise) . olWaveform) (iOscs crash))
  assertBool "ride should use noise envelopes" (any noiseLayerHasEnvelope (iOscs ride))

testGmProgramMapVariesByFamily ∷ IO ()
testGmProgramMapVariesByFamily = do
  let piano = gmProgramInstrument defaultMidiProgram
      guitar = gmProgramInstrument 24
      strings = gmProgramInstrument 48
      lead = gmProgramInstrument 80
  assertBool "piano and guitar patches should differ" (piano /= guitar)
  assertBool "guitar and strings patches should differ" (guitar /= strings)
  assertBool "strings and lead patches should differ" (strings /= lead)

testGmPercussionChannelIgnoresProgram ∷ IO ()
testGmPercussionChannelIgnoresProgram = do
  let drum0 = gmChannelInstrument gmPercussionChannel 0
      drum56 = gmChannelInstrument gmPercussionChannel 56
      melodic = gmChannelInstrument 0 56
  assertEqual "percussion patch should ignore program number" drum0 drum56
  assertBool "percussion patch should differ from melodic patch" (drum0 /= melodic)
  assertEqual "default percussion preload is kick-like" drum0 (gmDrumInstrument 36)

testGmDrumMapVariesByKeyFamily ∷ IO ()
testGmDrumMapVariesByKeyFamily = do
  let kick = gmDrumInstrument 36
      snare = gmDrumInstrument 38
      hatClosed = gmDrumInstrument 42
      hatOpen = gmDrumInstrument 46
      cowbell = gmDrumInstrument 56
      bongo = gmDrumInstrument 60
  assertBool "kick and snare patches should differ" (kick /= snare)
  assertBool "snare and hihat patches should differ" (snare /= hatClosed)
  assertBool "closed and open hihat patches should differ" (hatClosed /= hatOpen)
  assertBool "cowbell and bongo patches should differ" (cowbell /= bongo)
  assertEqual "kick patch should be short" 0 (aSustain (iAdsrDefault kick))

testInstrumentLoadKeepsActiveVoiceUntilLiveApply ∷ IO ()
testInstrumentLoadKeepsActiveVoiceUntilLiveApply = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      routesA = [ModRoute ModSrcLfo1 (ModDstLayerPitchCents 0) 7]
      routesB = [ModRoute ModSrcLfo1 ModDstFilterCutoffOct 0.5]
      instA =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSine rootPitch 1 NoSync Nothing]
          , iGain = 0.5
          , iModRoutes = routesA
          }
      instB =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSaw rootPitch 1 NoSync Nothing]
          , iGain = 0.9
          , iModRoutes = routesB
          }
  st1 <- setInstrument iid instA st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) (NoteInstanceId 1) 0.8 Nothing
  st3 <- setInstrument iid instB st2
  voiceLoaded <- MV.read (stVoices st3) 0
  oscLoaded <- MV.read (vOscs voiceLoaded) 0
  assertNear "loaded voice keeps snapshot gain" 0.5 (vInstrGain voiceLoaded)
  assertEqual "loaded voice keeps snapshot routes" routesA (vModRoutes voiceLoaded)
  assertEqual "loaded voice keeps original waveform" WaveSine (oWaveform oscLoaded)
  st4 <- applyInstrumentToActiveVoices testSampleRate iid instB st3
  voiceLive <- MV.read (stVoices st4) 0
  oscLive <- MV.read (vOscs voiceLive) 0
  assertNear "live apply updates snapshot gain" 0.9 (vInstrGain voiceLive)
  assertEqual "live apply updates snapshot routes" routesB (vModRoutes voiceLive)
  assertEqual "live apply updates waveform" WaveSaw (oWaveform oscLive)

testClipOneShotAutoReleasesSource ∷ IO ()
testClipOneShotAutoReleasesSource = do
  st0 <- mkTestState
  evQ <- newQueue
  let clip = ClipAsset 1 4 (VU.fromList [0.5, -0.5, 0.25, -0.25])
      src =
        ClipSource
          { csClipId = ClipId 0
          , csBus = AudioBusSfx
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
  MV.write (stClipAssets st0) 0 (Just clip)
  MV.write (stClipSources st0) 0 src
  let st1 = st0 { stClipActiveCount = 1 }
  bracket (rbCreateF32 64 2) rbDestroy $ \rb -> do
    (st2, wrote) <- renderChunkFrames evQ runScheduledForTest rb 8 st1
    assertEqual "clip chunk write count" 8 wrote
    assertEqual "one-shot should release source after end" 0 (stClipActiveCount st2)

testClipMusicBusGainZeroMutesOutput ∷ IO ()
testClipMusicBusGainZeroMutesOutput = do
  st0 <- mkTestState
  evQ <- newQueue
  let clip = ClipAsset 1 1 (VU.fromList [1])
      src =
        ClipSource
          { csClipId = ClipId 0
          , csBus = AudioBusMusic
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
  MV.write (stClipAssets st0) 0 (Just clip)
  MV.write (stClipSources st0) 0 src
  let st1 =
        st0
          { stClipActiveCount = 1
          , stBusMusicGain = 0
          }
  bracket (rbCreateF32 16 2) rbDestroy $ \rb -> do
    (_st2, _wrote) <- renderChunkFrames evQ runScheduledForTest rb 1 st1
    allocaArray 2 $ \buf -> do
      got <- rbReadF32 rb buf 1
      assertEqual "ring buffer should contain one frame" 1 (fromIntegral got ∷ Int)
      [l, r] <- peekArray 2 buf
      assertNear "left output should be muted by bus gain" 0 (realToFrac (l ∷ CFloat))
      assertNear "right output should be muted by bus gain" 0 (realToFrac (r ∷ CFloat))

testClipFinishedEmitsAudioEvent ∷ IO ()
testClipFinishedEmitsAudioEvent = do
  st0 <- mkTestState
  evQ <- newQueue
  let clip = ClipAsset 1 2 (VU.fromList [1, 0])
      src =
        ClipSource
          { csClipId = ClipId 42
          , csBus = AudioBusSfx
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
  MV.write (stClipAssets st0) 42 (Just clip)
  MV.write (stClipSources st0) 0 src
  let st1 = st0 { stClipActiveCount = 1 }
  bracket (rbCreateF32 16 2) rbDestroy $ \rb -> do
    (_st2, _wrote) <- renderChunkFrames evQ runScheduledForTest rb 8 st1
    events <- drainAudioEvents evQ
    assertBool
      "clip finished event should be emitted"
      (AudioEventClipFinished (ClipId 42) AudioBusSfx `elem` events)

testNoteFinishedEmitsAudioEvent ∷ IO ()
testNoteFinishedEmitsAudioEvent = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      noteInst = NoteInstanceId 99
      inst = (mkInstrument Poly) { iAdsrDefault = ADSR 0 0 0 0.001 }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid 1 0 (NoteKey 60) noteInst 0.8 Nothing
  st3 <- releaseInstrumentNote iid noteInst st2
  bracket (rbCreateF32 512 2) rbDestroy $ \rb -> do
    (_st4, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 512 st3
    events <- drainAudioEvents (audioEventQueue handle)
    assertBool
      "note finished event should be emitted"
      (AudioEventNoteFinished iid noteInst `elem` events)

testScheduledBusGainSwitchesAtFrame ∷ IO ()
testScheduledBusGainSwitchesAtFrame = do
  st0 <- mkTestState
  evQ <- newQueue
  let clip = ClipAsset 1 4 (VU.fromList [1, 1, 1, 1])
      src =
        ClipSource
          { csClipId = ClipId 0
          , csBus = AudioBusMusic
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
      sched = ScheduledItem 2 1 (ScheduledSetBusGain AudioBusMusic 0)
      st1 =
        st0
          { stClipActiveCount = 1
          , stScheduled = [sched]
          }
  MV.write (stClipAssets st1) 0 (Just clip)
  MV.write (stClipSources st1) 0 src
  bracket (rbCreateF32 16 2) rbDestroy $ \rb -> do
    (st2, _wrote) <- renderChunkFrames evQ runScheduledForTest rb 4 st1
    assertEqual "transport should advance by rendered frames" 4 (stTransportFrame st2)
    allocaArray 8 $ \buf -> do
      got <- rbReadF32 rb buf 4
      assertEqual "should read 4 rendered frames" 4 (fromIntegral got ∷ Int)
      vals <- map realToFrac <$> peekArray 8 buf
      case vals of
        [f0L, _f0R, f1L, _f1R, f2L, _f2R, f3L, _f3R] -> do
          assertBool "frames before schedule should be audible" (f0L > 0.9 && f1L > 0.9)
          assertNear "frame 2 should be muted after scheduled bus gain" 0 f2L
          assertNear "frame 3 should remain muted" 0 f3L
        _ ->
          error ("expected 8 samples from rendered frames, got " <> show (length vals))
    events <- drainAudioEvents evQ
    assertBool
      "schedule trigger event should be emitted"
      (AudioEventScheduleTriggered 2 (ScheduledSetBusGain AudioBusMusic 0) `elem` events)

testScheduledNoteOnOffTriggerAtFrames ∷ IO ()
testScheduledNoteOnOffTriggerAtFrames = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      noteInst = NoteInstanceId 77
      noteKey = NoteKey 60
      inst = (mkInstrument Poly) { iAdsrDefault = ADSR 0 0 1 0.5 }
      noteOnAction =
        ScheduledNoteOn
          { saInstrumentId = iid
          , saAmp = 1
          , saPan = 0
          , saNoteKey = noteKey
          , saNoteInstanceId = noteInst
          , saVelocity = 0.8
          , saAdsrOverride = Nothing
          }
      noteOffAction =
        ScheduledNoteOff
          { saInstrumentId = iid
          , saNoteInstanceId = noteInst
          }
  st1 <- setInstrument iid inst st0
  let st2 =
        st1
          { stScheduled =
              [ ScheduledItem 1 1 noteOnAction
              , ScheduledItem 3 2 noteOffAction
              ]
          }
  bracket (rbCreateF32 16 2) rbDestroy $ \rb -> do
    (st3, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st2
    assertEqual "transport should advance by rendered frames" 4 (stTransportFrame st3)
    assertEqual "scheduled note-on should allocate one active voice" 1 (stActiveCount st3)
    v <- MV.read (stVoices st3) 0
    assertEqual "voice should keep scheduled note instance id" (Just noteInst) (vNoteInstanceId v)
    assertEqual "scheduled note-off should move envelope to release stage" EnvRelease (eStage (vEnv v))
    events <- drainAudioEvents (audioEventQueue handle)
    assertBool
      "note-on schedule trigger event should be emitted"
      (AudioEventScheduleTriggered 1 noteOnAction `elem` events)
    assertBool
      "note-off schedule trigger event should be emitted"
      (AudioEventScheduleTriggered 3 noteOffAction `elem` events)

testWavDecoderLoadsStereoF32 ∷ IO ()
testWavDecoderLoadsStereoF32 =
  withTempWav [0, 32767, -32768, 16384] $ \path -> do
    decoded <- decodeAudioFileStereoF32 path
    assertEqual "decoded target channels" 2 (daChannels decoded)
    assertEqual "decoded frame count" 4 (daFrames decoded)
    let samples = daSamples decoded
    assertEqual "decoded sample count" 8 (length samples)
    assertNear "mono to stereo duplicates L/R 0" (samples !! 0) (samples !! 1)
    assertNear "mono to stereo duplicates L/R 1" (samples !! 2) (samples !! 3)
    assertNear "zero sample remains zero" 0 (samples !! 0)
    assertBool "max positive sample is normalized near 1" (samples !! 2 > 0.99)
    assertBool "negative sample is preserved" (samples !! 4 < -0.99)

withTempWav ∷ [Int] → (FilePath → IO a) → IO a
withTempWav monoSamples action = do
  tmpDir <- getTemporaryDirectory
  bracket (openBinaryTempFile tmpDir "idou-wav-loader-test.wav") cleanup $ \(path, h) -> do
    BL.hPut h (mkWav16Mono 48000 monoSamples)
    hClose h
    action path
  where
    cleanup (path, h) = do
      catchIgnore (hClose h)
      catchIgnore (removeFile path)

catchIgnore ∷ IO () → IO ()
catchIgnore io = do
  _ <- try io ∷ IO (Either SomeException ())
  pure ()

drainAudioEvents ∷ Queue AudioEvent → IO [AudioEvent]
drainAudioEvents q = go []
  where
    go acc = do
      m <- tryReadQueue q
      case m of
        Nothing -> pure (reverse acc)
        Just ev -> go (ev : acc)

runScheduledForTest ∷ ScheduledAudioAction → AudioState → IO AudioState
runScheduledForTest action st =
  case action of
    ScheduledPlayClip cid bus gain pan loop ->
      let (gainL, gainR) = panGains (max 0 gain) pan
          active = stClipActiveCount st
          cap = MV.length (stClipSources st)
      in if active >= cap
           then pure st
           else do
              MV.write
                (stClipSources st)
                active
                ClipSource
                  { csClipId = cid
                  , csBus = bus
                  , csGainL = gainL
                  , csGainR = gainR
                  , csFramePos = 0
                  , csLoop = loop
                  }
              pure st { stClipActiveCount = active + 1 }
    ScheduledNoteOn iid amp pan key noteInst vel adsrOverride -> do
      handle <- mkTestHandle
      addInstrumentNote handle st iid amp pan key noteInst vel adsrOverride
    ScheduledNoteOff iid noteInst ->
      releaseInstrumentNote iid noteInst st
    ScheduledStopClip cid -> do
      let vec = stClipSources st
          go i st'
            | i >= stClipActiveCount st' = pure st'
            | otherwise = do
                src <- MV.read vec i
                if csClipId src == cid
                  then do
                    let lastIx = stClipActiveCount st' - 1
                    when (i /= lastIx) $
                      MV.read vec lastIx >>= MV.write vec i
                    go i st' { stClipActiveCount = lastIx }
                  else go (i + 1) st'
      go 0 st
    ScheduledStopBus bus -> do
      let vec = stClipSources st
          go i st'
            | i >= stClipActiveCount st' = pure st'
            | otherwise = do
                src <- MV.read vec i
                if csBus src == bus
                  then do
                    let lastIx = stClipActiveCount st' - 1
                    when (i /= lastIx) $
                      MV.read vec lastIx >>= MV.write vec i
                    go i st' { stClipActiveCount = lastIx }
                  else go (i + 1) st'
      go 0 st
    ScheduledSetBusGain bus gain ->
      case bus of
        AudioBusMusic -> pure st { stBusMusicGain = max 0 (min 1 gain) }
        AudioBusSfx -> pure st { stBusSfxGain = max 0 (min 1 gain) }

mkWav16Mono ∷ Word32 → [Int] → BL.ByteString
mkWav16Mono sampleRate samples =
  toLazyByteString $
    string8 "RIFF"
      <> word32LE riffChunkSize
      <> string8 "WAVE"
      <> string8 "fmt "
      <> word32LE 16
      <> word16LE 1
      <> word16LE 1
      <> word32LE sampleRate
      <> word32LE byteRate
      <> word16LE blockAlign
      <> word16LE 16
      <> string8 "data"
      <> word32LE dataBytes
      <> mconcat (map (int16LE . fromIntegral) samples)
  where
    sampleCount = length samples
    dataBytes = fromIntegral (sampleCount * 2) ∷ Word32
    riffChunkSize = 36 + dataBytes
    blockAlign = 2
    byteRate = sampleRate * fromIntegral blockAlign

mkTestHandle ∷ IO AudioHandle
mkTestHandle = do
  q <- newQueue
  evQ <- newQueue
  pure AudioHandle
    { audioQueue = q
    , audioEventQueue = evQ
    , sampleRate = testSampleRate
    , channels = 2
    }

mkTestState ∷ IO AudioState
mkTestState = do
  voices <- MV.new 8
  clipAssetVec <- MV.replicate 1024 Nothing
  clipSourceVec <- MV.new 8
  mixBuf <- mallocForeignPtrArray (256 * 2)
  instVec <- MV.replicate 256 Nothing
  glideVec <- MV.replicate 256 0
  legFiltRetrigVec <- MV.replicate 256 False
  legAmpRetrigVec <- MV.replicate 256 False
  vibRateVec <- MV.replicate 256 0
  vibDepthVec <- MV.replicate 256 0
  channelVolumeVec <- MV.replicate 256 1
  channelExpressionVec <- MV.replicate 256 1
  channelPanVec <- MV.replicate 256 0
  modWheelVec <- MV.replicate 256 0
  channelAftertouchVec <- MV.replicate 256 0
  pitchBendVec <- MV.replicate 256 0
  pitchModScratch <- MV.replicate 4 1
  pitchCentsScratch <- MV.replicate 4 0
  pure AudioState
    { stVoices = voices
    , stActiveCount = 0
    , stClipAssets = clipAssetVec
    , stClipSources = clipSourceVec
    , stClipActiveCount = 0
    , stBusMusicGain = 1
    , stBusSfxGain = 1
    , stMixBuf = mixBuf
    , stSampleRate = testSampleRate
    , stTargetBufferFrames = 2048
    , stPitchModScratch = pitchModScratch
    , stPitchCentsScratch = pitchCentsScratch
    , stInstruments = instVec
    , stGlideSec = glideVec
    , stLegFiltRetrig = legFiltRetrigVec
    , stLegAmpRetrig = legAmpRetrigVec
    , stVibRateHz = vibRateVec
    , stVibDepthCents = vibDepthVec
    , stChannelVolume = channelVolumeVec
    , stChannelExpression = channelExpressionVec
    , stChannelPan = channelPanVec
    , stModWheel = modWheelVec
    , stChannelAftertouch = channelAftertouchVec
    , stPitchBendSemis = pitchBendVec
    , stNow = 0
    , stTransportFrame = 0
    , stTransportBpm = 120
    , stScheduleSeq = 0
    , stScheduled = []
    }

mkInstrument ∷ PlayMode → Instrument
mkInstrument playMode =
  Instrument
    { iOscs = [OscLayer WaveSine rootPitch 1 NoSync Nothing]
    , iLayerSpread = 0
    , iAdsrDefault = ADSR 0.01 0.02 0.5 0.03
    , iGain = 1
    , iFilter = Nothing
    , iModRoutes = []
    , iPlayMode = playMode
    , iPolyMax = 8
    , iVoiceSteal = StealQuietest
    }

rootPitch ∷ PitchSpec
rootPitch = PitchSpec 0 0 0 0

readActiveVoices ∷ AudioState → IO [Voice]
readActiveVoices st =
  mapM (MV.read (stVoices st)) [0 .. stActiveCount st - 1]

findVoice ∷ NoteInstanceId → [Voice] → IO Voice
findVoice instId voices =
  case find ((== Just instId) . vNoteInstanceId) voices of
    Just voice -> pure voice
    Nothing -> error ("missing voice with instance " <> show instId)

testSampleRate ∷ Word32
testSampleRate = 48000

stepEnvN ∷ Int → ADSR → EnvState → EnvState
stepEnvN n adsr = go n
  where
    go steps st
      | steps <= 0 = st
      | otherwise =
          let (st', _) = envStep (fromIntegral testSampleRate) adsr st
          in go (steps - 1) st'

takeOscSamples ∷ Int → Osc → [Float]
takeOscSamples n = go n []
  where
    go remaining acc osc0
      | remaining <= 0 = reverse acc
      | otherwise =
          let (osc1, sample) = oscStep osc0
          in go (remaining - 1) (sample : acc) osc1

meanAbs ∷ [Float] → Float
meanAbs [] = 0
meanAbs xs = sum (map abs xs) / fromIntegral (length xs)

meanAbsDelta ∷ [Float] → Float
meanAbsDelta [] = 0
meanAbsDelta [_] = 0
meanAbsDelta (x:y:rest) = go y (abs (y - x)) 1 rest
  where
    go ∷ Float → Float → Int → [Float] → Float
    go _ acc count [] = acc / fromIntegral count
    go prev acc count (z:zs) = go z (acc + abs (z - prev)) (count + 1) zs

waveformIs ∷ (Waveform → Bool) → Waveform → Bool
waveformIs predicate wf = predicate wf

isNoiseMix ∷ Waveform → Bool
isNoiseMix wf =
  case wf of
    WaveNoiseMix _ -> True
    _ -> False

isNoiseWaveform ∷ Waveform → Bool
isNoiseWaveform wf =
  case wf of
    WaveWhiteNoise -> True
    WavePinkNoise -> True
    WaveNoiseMix _ -> True
    _ -> False

noiseLayerHasEnvelope ∷ OscLayer → Bool
noiseLayerHasEnvelope layer =
  isNoiseWaveform (olWaveform layer) && olAmpEnv layer /= Nothing

hasModSrc ∷ ModSrc → [ModRoute] → Bool
hasModSrc src = any ((== src) . mrSrc)

assertBool ∷ String → Bool → IO ()
assertBool label cond =
  unless cond (error label)

assertEqual ∷ (Eq a, Show a) => String → a → a → IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    error (label <> ": expected " <> show expected <> ", got " <> show actual)

assertNear ∷ String → Float → Float → IO ()
assertNear label expected actual =
  unless (abs (expected - actual) < 1e-4) $
    error (label <> ": expected " <> show expected <> ", got " <> show actual)
