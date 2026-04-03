{-# LANGUAGE Strict, UnicodeSyntax #-}

module Tests.Audio (testCases) where

import Control.Exception (SomeException, bracket, displayException, evaluate, try)
import Control.Monad (foldM)
import Data.List (isInfixOf, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Foreign.C (CFloat)
import Foreign.Marshal.Array (allocaArray, peekArray)

import Audio.Envelope
import Audio.Filter.Biquad (FilterType(..))
import Audio.Filter.Types (FilterSpec(..), FilterSlope(..), FilterTarget(..), KeyTrack(..))
import Audio.Oscillator (Osc(..), oscInitSeeded, oscStep)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument, gmDrumInstrument, gmProgramInstrument, gmPercussionChannel)
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
import Audio.Thread.Render (RenderStats(..), renderChunkFrames, renderIfNeeded)
import Audio.Thread.Types (AudioState(..), ClipAsset(..), ClipSource(..), ScheduledItem(..), Voice(..))
import Audio.Thread.Voice
  ( addInstrumentNote
  , applyInstrumentToActiveVoices
  , releaseInstrumentAllVoices
  , releaseInstrumentNote
  , setInstrumentNoteAftertouch
  )
import Audio.Types
import Engine.Core.Queue (newQueue)
import Midi.Control (controllerPanValue, controllerValue01, midiPitchBendSemitones, sustainPedalDown)
import Midi.Play.Util
  ( enqueueDeferredRelease
  , popHeldNote
  , pushHeldNote
  , takeDeferredReleases
  , ticksToMicroseconds
  , ticksToMicrosecondsEither
  )
import Sound.Miniaudio.Decode (DecodedAudio(..), decodeAudioFileStereoF32)
import Sound.Miniaudio.RingBuffer (rbCreateF32, rbDestroy, rbReadF32)
import TestSupport
  ( TestCase(..)
  , assertBool
  , assertEqual
  , assertNear
  , drainAudioEvents
  , findVoice
  , hasModSrc
  , isNoiseMix
  , isNoiseWaveform
  , meanAbs
  , meanAbsDelta
  , mkInstrument
  , mkTestHandle
  , mkTestState
  , mkTestStateWithVoices
  , noiseLayerHasEnvelope
  , readActiveVoices
  , readRenderedSamples
  , rootPitch
  , runScheduledForTest
  , stepEnvN
  , takeEveryOther
  , takeOscSamples
  , testSampleRate
  , waveformIs
  , withTempWav
  )

testCases ∷ [TestCase]
testCases =
  [ TestCase "envelope-release-reaches-done" testEnvelopeReleaseReachesDone
  , TestCase "mono-adsr-override-on-new-voice" testMonoAdsrOverrideOnNewVoice
  , TestCase "mono-legato-reuses-voice-and-updates-override" testMonoLegatoReusesVoiceAndUpdatesOverride
  , TestCase "mono-legato-sustain-style-stale-release-is-ignored" testMonoLegatoSustainStyleStaleReleaseIsIgnored
  , TestCase "mono-legato-sustain-style-current-release-still-works" testMonoLegatoSustainStyleCurrentReleaseStillWorks
  , TestCase "release-targets-matching-note-instance" testReleaseTargetsMatchingNoteInstance
  , TestCase "poly-voice-steal-reuses-released-slot" testPolyVoiceStealReusesReleasedSlot
  , TestCase "dense-polyphony-renders-across-many-voices" testDensePolyphonyRendersAcrossManyVoices
  , TestCase "render-if-needed-reports-load-metrics" testRenderIfNeededReportsLoadMetrics
  , TestCase "release-all-voices-keeps-other-instruments-active" testReleaseAllVoicesKeepsOtherInstrumentsActive
  , TestCase "midi-controller-state-clamps-and-resets" testMidiControllerStateClampsAndResets
  , TestCase "midi-controller-conversions-are-normalized" testMidiControllerConversionsAreNormalized
  , TestCase "midi-held-notes-release-oldest-instance-first" testMidiHeldNotesReleaseOldestInstanceFirst
  , TestCase "midi-deferred-releases-preserve-noteoff-order" testMidiDeferredReleasesPreserveNoteOffOrder
  , TestCase "midi-tick-conversion-basic" testMidiTickConversionBasic
  , TestCase "midi-tick-conversion-either-basic" testMidiTickConversionEitherBasic
  , TestCase "midi-tick-conversion-either-rejects-zero-ppqn" testMidiTickConversionEitherRejectsZeroPpqn
  , TestCase "midi-tick-conversion-rejects-zero-ppqn" testMidiTickConversionRejectsZeroPpqn
  , TestCase "poly-aftertouch-targets-matching-note-instance" testPolyAftertouchTargetsMatchingNoteInstance
  , TestCase "aftertouch-routes-are-wired-into-expressive-patches" testAftertouchRoutesAreWiredIntoExpressivePatches
  , TestCase "noise-waveforms-mix-between-white-and-pink" testNoiseWaveformsMixBetweenWhiteAndPink
  , TestCase "pink-noise-is-smoother-than-white" testPinkNoiseIsSmootherThanWhite
  , TestCase "noise-layer-envelope-shapes-amplitude" testNoiseLayerEnvelopeShapesAmplitude
  , TestCase "gm-drum-noise-layers-use-envelopes" testGmDrumNoiseLayersUseEnvelopes
  , TestCase "gm-kick-uses-separate-body-and-attack-tuning" testGmKickUsesSeparateBodyAndAttackTuning
  , TestCase "gm-tom-family-has-noise-attack-and-ascending-tuning" testGmTomFamilyHasNoiseAttackAndAscendingTuning
  , TestCase "gm-crash-has-long-noise-tail" testGmCrashHasLongNoiseTail
  , TestCase "gm-program-map-varies-by-family" testGmProgramMapVariesByFamily
  , TestCase "gm-percussion-channel-ignores-program" testGmPercussionChannelIgnoresProgram
  , TestCase "gm-drum-map-varies-by-key-family" testGmDrumMapVariesByKeyFamily
  , TestCase "gm-drum-map-covers-standard-toms-and-cymbals" testGmDrumMapCoversStandardTomsAndCymbals
  , TestCase "patch-lfo-rate-drives-autonomous-lfo-phase" testPatchLfoRateDrivesAutonomousLfoPhase
  , TestCase "filter-target-routes-only-selected-layer" testFilterTargetRoutesOnlySelectedLayer
  , TestCase "instrument-load-keeps-active-voice-until-live-apply" testInstrumentLoadKeepsActiveVoiceUntilLiveApply
  , TestCase "clip-one-shot-auto-releases-source" testClipOneShotAutoReleasesSource
  , TestCase "clip-music-bus-gain-zero-mutes-output" testClipMusicBusGainZeroMutesOutput
  , TestCase "note-sfx-bus-gain-zero-mutes-output" testNoteSfxBusGainZeroMutesOutput
  , TestCase "clip-finished-emits-audio-event" testClipFinishedEmitsAudioEvent
  , TestCase "note-finished-emits-audio-event" testNoteFinishedEmitsAudioEvent
  , TestCase "mixed-song-and-wav-clip-coexist-on-music-bus" testMixedSongAndWavClipCoexistOnMusicBus
  , TestCase "mixed-song-and-sfx-note-respect-separate-buses" testMixedSongAndSfxNoteRespectSeparateBuses
  , TestCase "mixed-song-and-midi-note-share-music-bus" testMixedSongAndMidiNoteShareMusicBus
  , TestCase "scheduled-bus-gain-switches-at-frame" testScheduledBusGainSwitchesAtFrame
  , TestCase "scheduled-stop-bus-clears-note-and-clip-sources" testScheduledStopBusClearsNoteAndClipSources
  , TestCase "scheduled-note-on-off-trigger-at-frames" testScheduledNoteOnOffTriggerAtFrames
  , TestCase "wav-decoder-loads-stereo-f32" testWavDecoderLoadsStereoF32
  , TestCase "wav-decoder-reports-missing-file" testWavDecoderReportsMissingFile
  ]

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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) (NoteInstanceId 1) 0.8 (Just override)
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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) (NoteInstanceId 1) 0.7 (Just override1)
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 67) (NoteInstanceId 2) 0.7 (Just override2)
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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 67) secondId 0.8 Nothing
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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 67) secondId 0.8 Nothing
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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 64) secondId 0.8 Nothing
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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 64) secondId 0.8 Nothing
  st4 <- releaseInstrumentNote iid firstId st3
  st5 <- addInstrumentNote handle st4 iid AudioBusMusic 1 0 (NoteKey 67) thirdId 0.8 Nothing
  voices <- readActiveVoices st5
  assertEqual "active voice count after steal" 2 (stActiveCount st5)
  assertBool "released voice should be replaced" (all ((/= Just firstId) . vNoteInstanceId) voices)
  assertBool "second voice should survive steal" (any ((== Just secondId) . vNoteInstanceId) voices)
  assertBool "new voice should be present" (any ((== Just thirdId) . vNoteInstanceId) voices)

testDensePolyphonyRendersAcrossManyVoices ∷ IO ()
testDensePolyphonyRendersAcrossManyVoices = do
  handle <- mkTestHandle
  st0 <- mkTestStateWithVoices 64
  let iid = InstrumentId 0
      voiceCount = 32
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSaw rootPitch 1 NoSync Nothing]
          , iPolyMax = voiceCount
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      addDenseVoice st ix =
        addInstrumentNote
          handle
          st
          iid
          AudioBusMusic
          0.04
          0
          (NoteKey (48 + (ix `mod` 24)))
          (NoteInstanceId (fromIntegral ix + 1))
          0.8
          Nothing
  st1 <- setInstrument iid inst st0
  st2 <- foldM addDenseVoice st1 [0 .. voiceCount - 1]
  assertEqual "dense polyphony should allocate all requested voices" voiceCount (stActiveCount st2)
  assertEqual "dense polyphony should advance the note timestamp per voice" (fromIntegral voiceCount) (stNow st2)
  voices <- readActiveVoices st2
  let instances = mapMaybe vNoteInstanceId voices
  assertEqual "dense polyphony should keep every voice instance unique" voiceCount (length (nub instances))
  assertBool "dense polyphony should keep all voices on the target instrument" (all ((== Just iid) . vInstrId) voices)
  bracket (rbCreateF32 64 2) rbDestroy $ \rb -> do
    (_st3, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 8 st2
    samples <- takeEveryOther <$> readRenderedSamples rb 8
    assertBool "dense polyphony should still render audible output" (meanAbs samples > 0.02)

testRenderIfNeededReportsLoadMetrics ∷ IO ()
testRenderIfNeededReportsLoadMetrics = do
  handle <- mkTestHandle
  st0 <- mkTestStateWithVoices 16
  let iid = InstrumentId 0
      voiceCount = 6
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSaw rootPitch 1 NoSync Nothing]
          , iPolyMax = voiceCount
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      addVoice st ix =
        addInstrumentNote
          handle
          st
          iid
          AudioBusMusic
          0.05
          0
          (NoteKey (60 + ix))
          (NoteInstanceId (fromIntegral ix + 1))
          0.85
          Nothing
  st1 <- setInstrument iid inst st0
  st2 <- foldM addVoice st1 [0 .. voiceCount - 1]
  bracket (rbCreateF32 64 2) rbDestroy $ \rb -> do
    let stTargeted = st2 { stTargetBufferFrames = 16 }
    (_st3, stats) <- renderIfNeeded (audioEventQueue handle) runScheduledForTest rb 8 stTargeted
    assertEqual "render should refill to the target buffer" 16 (rsFramesWritten stats)
    assertEqual "render load should capture peak active voices" voiceCount (rsPeakActiveVoices stats)
    assertEqual "render load should capture peak active clip sources" 0 (rsPeakActiveClips stats)
    assertBool "render load should record a non-negative mix duration" (rsMixDurationUs stats >= 0)

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
  st3 <- addInstrumentNote handle st2 iid0 AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st4 <- addInstrumentNote handle st3 iid0 AudioBusMusic 1 0 (NoteKey 64) secondId 0.8 Nothing
  st5 <- addInstrumentNote handle st4 iid1 AudioBusMusic 1 0 (NoteKey 67) thirdId 0.8 Nothing
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

testMidiTickConversionEitherBasic ∷ IO ()
testMidiTickConversionEitherBasic =
  assertEqual
    "safe tick conversion should match tempo math"
    (Right 250000)
    (ticksToMicrosecondsEither 480 500000 240)

testMidiTickConversionEitherRejectsZeroPpqn ∷ IO ()
testMidiTickConversionEitherRejectsZeroPpqn =
  assertEqual
    "safe tick conversion should reject zero ppqn"
    (Left "ticksToMicroseconds: ppqn must be greater than 0")
    (ticksToMicrosecondsEither 0 500000 240)

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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) firstId 0.8 Nothing
  st3 <- addInstrumentNote handle st2 iid AudioBusMusic 1 0 (NoteKey 64) secondId 0.8 Nothing
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

testGmKickUsesSeparateBodyAndAttackTuning ∷ IO ()
testGmKickUsesSeparateBodyAndAttackTuning = do
  let kick = gmDrumInstrument 36
  case iOscs kick of
    (bodyLayer : _bodyTopLayer : triggerNoiseLayer : attackLayer : _) -> do
      let bodyTune = psSemitones (olPitch bodyLayer)
          attackTune = psSemitones (olPitch attackLayer)
      assertBool "kick body tuning should be low" (bodyTune < (-12))
      assertBool "kick attack should be tuned above body" (attackTune > bodyTune + 20)
      assertEqual "kick trigger should use white noise" WaveWhiteNoise (olWaveform triggerNoiseLayer)
      assertBool "kick trigger should use a short envelope" (olAmpEnv triggerNoiseLayer /= Nothing)
    _ ->
      error "kick patch should expose body, trigger, and attack layers"

testGmTomFamilyHasNoiseAttackAndAscendingTuning ∷ IO ()
testGmTomFamilyHasNoiseAttackAndAscendingTuning = do
  let toms = map gmDrumInstrument [41, 43, 45, 47, 48, 50]
      bodySemis = map primaryLayerSemitone toms
  assertBool
    "tom body tuning should rise across the GM tom family"
    (and (zipWith (<) bodySemis (drop 1 bodySemis)))
  assertBool
    "each tom should include an enveloped noise attack layer"
    (all (any noiseLayerHasEnvelope . iOscs) toms)
  where
    primaryLayerSemitone inst =
      case iOscs inst of
        layer : _ -> psSemitones (olPitch layer)
        [] -> error "expected tom patch to have at least one oscillator layer"

testGmCrashHasLongNoiseTail ∷ IO ()
testGmCrashHasLongNoiseTail = do
  let crash = gmDrumInstrument 49
      noiseLayers = filter (isNoiseWaveform . olWaveform) (iOscs crash)
  assertBool "crash should use multiple noise layers" (length noiseLayers >= 3)
  assertBool "crash should have a long release tail" (aReleaseSec (iAdsrDefault crash) > 0.8)
  assertBool "crash should allow many overlapping hits" (iPolyMax crash >= 64)

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

testGmDrumMapCoversStandardTomsAndCymbals ∷ IO ()
testGmDrumMapCoversStandardTomsAndCymbals = do
  let lowFloorTom = gmDrumInstrument 41
      highTom = gmDrumInstrument 50
      crash1 = gmDrumInstrument 49
      china = gmDrumInstrument 52
      splash = gmDrumInstrument 55
      crash2 = gmDrumInstrument 57
      ride1 = gmDrumInstrument 51
      ride2 = gmDrumInstrument 59
      clap = gmDrumInstrument 39
      vibraslap = gmDrumInstrument 58
  assertBool "floor tom and high tom patches should differ" (lowFloorTom /= highTom)
  assertBool "china cymbal should differ from crash 1" (china /= crash1)
  assertBool "splash cymbal should differ from crash 2" (splash /= crash2)
  assertBool "ride 1 and ride 2 should differ" (ride1 /= ride2)
  assertBool "vibraslap should not reuse clap patch" (vibraslap /= clap)

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
          , iLfo1RateHz = 3.5
          , iModRoutes = routesA
          }
      instB =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSaw rootPitch 1 NoSync Nothing]
          , iGain = 0.9
          , iLfo1RateHz = 6.25
          , iModRoutes = routesB
          }
  st1 <- setInstrument iid instA st0
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) (NoteInstanceId 1) 0.8 Nothing
  st3 <- setInstrument iid instB st2
  voiceLoaded <- MV.read (stVoices st3) 0
  oscLoaded <- MV.read (vOscs voiceLoaded) 0
  assertNear "loaded voice keeps snapshot gain" 0.5 (vInstrGain voiceLoaded)
  assertNear "loaded voice keeps snapshot lfo rate" 3.5 (vLfo1RateHz voiceLoaded)
  assertEqual "loaded voice keeps snapshot routes" routesA (vModRoutes voiceLoaded)
  assertEqual "loaded voice keeps original waveform" WaveSine (oWaveform oscLoaded)
  st4 <- applyInstrumentToActiveVoices testSampleRate iid instB st3
  voiceLive <- MV.read (stVoices st4) 0
  oscLive <- MV.read (vOscs voiceLive) 0
  assertNear "live apply updates snapshot gain" 0.9 (vInstrGain voiceLive)
  assertNear "live apply updates snapshot lfo rate" 6.25 (vLfo1RateHz voiceLive)
  assertEqual "live apply updates snapshot routes" routesB (vModRoutes voiceLive)
  assertEqual "live apply updates waveform" WaveSaw (oWaveform oscLive)

testPatchLfoRateDrivesAutonomousLfoPhase ∷ IO ()
testPatchLfoRateDrivesAutonomousLfoPhase = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSine rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          , iLfo1RateHz = 6
          , iModRoutes = [ModRoute ModSrcLfo1 ModDstAmpGain 0.2]
          }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) (NoteInstanceId 81) 0.8 Nothing
  voiceBefore <- MV.read (stVoices st2) 0
  assertNear "voice should start with zero lfo phase" 0 (vLfo1Phase voiceBefore)
  bracket (rbCreateF32 256 2) rbDestroy $ \rb -> do
    (_st3, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 64 st2
    vals <- takeEveryOther <$> readRenderedSamples rb 64
    assertBool "autonomous patch lfo should still render audible output" (meanAbs vals > 0.001)
    voiceAfter <- MV.read (stVoices _st3) 0
    assertBool "patch-local lfo rate should advance phase without mod wheel or vibrato" (vLfo1Phase voiceAfter > 0)

testFilterTargetRoutesOnlySelectedLayer ∷ IO ()
testFilterTargetRoutesOnlySelectedLayer = do
  let renderMean target = do
        handle <- mkTestHandle
        st0 <- mkTestState
        let iid = InstrumentId 0
            filt =
              FilterSpec
                { fType = FLP
                , fCutoffHz = 180
                , fQ = 0.707
                , fSlope = S24
                , fKeyTrack = KeyTrack 0
                , fTarget = target
                , fEnvAmountOct = 0
                , fEnvADSR = ADSR 0 0 1 0.1
                , fQEnvAmount = 0
                }
            inst =
              (mkInstrument Poly)
                { iOscs =
                    [ OscLayer WaveSine rootPitch 0.12 NoSync Nothing
                    , OscLayer WaveSquare (PitchSpec 0 0 0 4000) 1 NoSync Nothing
                    ]
                , iAdsrDefault = ADSR 0 0 1 0.3
                , iFilter = Just filt
                }
        st1 <- setInstrument iid inst st0
        st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) (NoteInstanceId 9) 0.8 Nothing
        bracket (rbCreateF32 256 2) rbDestroy $ \rb -> do
          (_st3, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 96 st2
          meanAbs . takeEveryOther <$> readRenderedSamples rb 96
  meanAll <- renderMean FilterTargetAll
  meanLayer0 <- renderMean (FilterTargetLayer 0)
  assertBool
    "targeting the filter at layer 0 should leave the bright second layer dry and louder than filtering all layers"
    (meanLayer0 > meanAll * 1.5)

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

testNoteSfxBusGainZeroMutesOutput ∷ IO ()
testNoteSfxBusGainZeroMutesOutput = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid AudioBusSfx 1 0 (NoteKey 60) (NoteInstanceId 1) 0.8 Nothing
  let st3 = st2 { stBusSfxGain = 0 }
  bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
    (_st4, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st3
    vals <- readRenderedSamples rb 4
    assertBool "all note samples should be muted by SFX bus gain" (all (\x -> abs x < 1e-6) vals)

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
  st2 <- addInstrumentNote handle st1 iid AudioBusMusic 1 0 (NoteKey 60) noteInst 0.8 Nothing
  st3 <- releaseInstrumentNote iid noteInst st2
  bracket (rbCreateF32 512 2) rbDestroy $ \rb -> do
    (_st4, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 512 st3
    events <- drainAudioEvents (audioEventQueue handle)
    assertBool
      "note finished event should be emitted"
      (AudioEventNoteFinished iid noteInst `elem` events)

testMixedSongAndWavClipCoexistOnMusicBus ∷ IO ()
testMixedSongAndWavClipCoexistOnMusicBus = do
  handle <- mkTestHandle
  let iid = InstrumentId 0
      noteInst = NoteInstanceId 70
      clipId' = ClipId 7
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      noteOnAction =
        ScheduledNoteOn
          { saInstrumentId = iid
          , saNoteBus = AudioBusMusic
          , saAmp = 0.4
          , saPan = 0
          , saNoteKey = NoteKey 60
          , saNoteInstanceId = noteInst
          , saVelocity = 0.8
          , saAdsrOverride = Nothing
          }
      clip =
        ClipAsset 1 4 (VU.fromList [0.25, 0.25, 0.25, 0.25])
      clipSrc =
        ClipSource
          { csClipId = clipId'
          , csBus = AudioBusMusic
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
      renderScenario includeSong includeClip = do
        st0 <- mkTestState
        st1 <- setInstrument iid inst st0
        let st2 =
              st1
                { stScheduled = if includeSong then [ScheduledItem 0 1 noteOnAction] else []
                }
        if includeClip
          then do
            MV.write (stClipAssets st2) 7 (Just clip)
            MV.write (stClipSources st2) 0 clipSrc
            let st3 = st2 { stClipActiveCount = 1 }
            bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
              (_st4, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st3
              takeEveryOther <$> readRenderedSamples rb 4
          else
            bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
              (_st3, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st2
              takeEveryOther <$> readRenderedSamples rb 4
  songOnly <- renderScenario True False
  clipOnly <- renderScenario False True
  mixed <- renderScenario True True
  assertBool
    "music note and clip should both contribute on the shared music bus"
    (meanAbs mixed > meanAbs songOnly && meanAbs mixed > meanAbs clipOnly)

testMixedSongAndSfxNoteRespectSeparateBuses ∷ IO ()
testMixedSongAndSfxNoteRespectSeparateBuses = do
  handle <- mkTestHandle
  let songIid = InstrumentId 0
      sfxIid = InstrumentId 1
      songInst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      sfxInst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      songAction =
        ScheduledNoteOn
          { saInstrumentId = songIid
          , saNoteBus = AudioBusMusic
          , saAmp = 0.5
          , saPan = 0
          , saNoteKey = NoteKey 60
          , saNoteInstanceId = NoteInstanceId 71
          , saVelocity = 0.8
          , saAdsrOverride = Nothing
          }
      renderScenario includeSfx = do
        st0 <- mkTestState
        st1 <- setInstrument songIid songInst st0
        st2 <- setInstrument sfxIid sfxInst st1
        st3 <-
          if includeSfx
            then addInstrumentNote handle st2 sfxIid AudioBusSfx 0.5 0 (NoteKey 67) (NoteInstanceId 72) 0.8 Nothing
            else pure st2
        let st4 =
              st3
                { stScheduled = [ScheduledItem 0 1 songAction]
                , stBusSfxGain = 0
                }
        bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
          (_st5, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st4
          takeEveryOther <$> readRenderedSamples rb 4
  songOnly <- renderScenario False
  songWithMutedSfx <- renderScenario True
  assertBool
    "muting SFX bus should leave the scheduled song note unchanged"
    (and (zipWith (\a b -> abs (a - b) < 1e-5) songOnly songWithMutedSfx))

testMixedSongAndMidiNoteShareMusicBus ∷ IO ()
testMixedSongAndMidiNoteShareMusicBus = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let songIid = InstrumentId 0
      midiIid = InstrumentId 1
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      songAction =
        ScheduledNoteOn
          { saInstrumentId = songIid
          , saNoteBus = AudioBusMusic
          , saAmp = 0.35
          , saPan = 0
          , saNoteKey = NoteKey 60
          , saNoteInstanceId = NoteInstanceId 73
          , saVelocity = 0.8
          , saAdsrOverride = Nothing
          }
  st1 <- setInstrument songIid inst st0
  st2 <- setInstrument midiIid inst st1
  st3 <- addInstrumentNote handle st2 midiIid AudioBusMusic 0.35 0 (NoteKey 64) (NoteInstanceId 74) 0.8 Nothing
  let st4 =
        st3
          { stScheduled = [ScheduledItem 0 1 songAction]
          , stBusMusicGain = 0
          }
  bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
    (_st5, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st4
    vals <- readRenderedSamples rb 4
    assertBool "muting music bus should silence both scheduled song and MIDI-style note" (all (\x -> abs x < 1e-6) vals)

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
          , saNoteBus = AudioBusMusic
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

testScheduledStopBusClearsNoteAndClipSources ∷ IO ()
testScheduledStopBusClearsNoteAndClipSources = do
  handle <- mkTestHandle
  st0 <- mkTestState
  let iid = InstrumentId 0
      inst =
        (mkInstrument Poly)
          { iOscs = [OscLayer WaveSquare rootPitch 1 NoSync Nothing]
          , iAdsrDefault = ADSR 0 0 1 0.5
          }
      clip =
        ClipAsset 1 8 (VU.fromList (replicate 8 0.25))
      clipSrc =
        ClipSource
          { csClipId = ClipId 3
          , csBus = AudioBusSfx
          , csGainL = 1
          , csGainR = 1
          , csFramePos = 0
          , csLoop = False
          }
  st1 <- setInstrument iid inst st0
  st2 <- addInstrumentNote handle st1 iid AudioBusSfx 0.5 0 (NoteKey 60) (NoteInstanceId 75) 0.8 Nothing
  MV.write (stClipAssets st2) 3 (Just clip)
  MV.write (stClipSources st2) 0 clipSrc
  let st3 =
        st2
          { stClipActiveCount = 1
          , stScheduled = [ScheduledItem 1 1 (ScheduledStopBus AudioBusSfx)]
          }
  bracket (rbCreateF32 32 2) rbDestroy $ \rb -> do
    (st4, _wrote) <- renderChunkFrames (audioEventQueue handle) runScheduledForTest rb 4 st3
    assertEqual "scheduled stop bus should remove SFX note voices" 0 (stActiveCount st4)
    assertEqual "scheduled stop bus should remove SFX clips" 0 (stClipActiveCount st4)
    events <- drainAudioEvents (audioEventQueue handle)
    assertBool
      "stop-bus trigger event should be emitted"
      (AudioEventScheduleTriggered 1 (ScheduledStopBus AudioBusSfx) `elem` events)

testWavDecoderLoadsStereoF32 ∷ IO ()
testWavDecoderLoadsStereoF32 =
  withTempWav [0, 32767, -32768, 16384] $ \path -> do
    decoded <- decodeAudioFileStereoF32 path
    assertEqual "decoded target channels" 2 (daChannels decoded)
    assertEqual "decoded frame count" 4 (daFrames decoded)
    let samples = daSamples decoded
    assertEqual "decoded sample count" 8 (VU.length samples)
    assertNear "mono to stereo duplicates L/R 0" (samples VU.! 0) (samples VU.! 1)
    assertNear "mono to stereo duplicates L/R 1" (samples VU.! 2) (samples VU.! 3)
    assertNear "zero sample remains zero" 0 (samples VU.! 0)
    assertBool "max positive sample is normalized near 1" (samples VU.! 2 > 0.99)
    assertBool "negative sample is preserved" (samples VU.! 4 < -0.99)

testWavDecoderReportsMissingFile ∷ IO ()
testWavDecoderReportsMissingFile = do
  let path = "/definitely/missing/idou-test-missing.wav"
  result <- try (decodeAudioFileStereoF32 path) ∷ IO (Either SomeException DecodedAudio)
  case result of
    Left ex ->
      assertBool
        "expected missing wav decode error"
        (("decodeAudioFileF32 failed for " <> path) `isInfixOf` displayException ex)
    Right decoded ->
      error ("expected missing wav decode failure, got " <> show decoded)
