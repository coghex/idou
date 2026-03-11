{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (forM, unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.List (find, intercalate, isInfixOf, nub)
import qualified Data.Map.Strict as M
import Data.Word (Word32, Word64)
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
import Player.Automation
  ( AutomationCurve(..)
  , EnergyLaneEvent(..)
  , MoodLaneEvent(..)
  , barFramesForTransport
  , scheduleEnergyLaneNextBar
  , scheduleMoodLaneNextBar
  , stepEnergyLane
  , stepMoodLane
  )
import Player.Timeline
  ( SongMode(..)
  , SongSpec
  , TimelineBar(..)
  , TimelineNote(..)
  , TimelineTransitionTelemetry(..)
  , TimelineRuntime(..)
  , compileTimelineBars
  , parseSongSpecText
  , setTimelineGenre
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeEndFrame
  , popTransitionTelemetry
  , popReadyBars
  , prepareTimelineRuntime
  , setTimelineTargets
  , sgGenre
  , sgInstruments
  , sgLookaheadBars
  , sgMode
  , sgSections
  , timelineRuntimeDone
  )
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
  , TestCase "timeline-song-spec-parses-yaml" testTimelineSongSpecParsesYaml
  , TestCase "timeline-song-intent-schema-arranges-electronic-song" testTimelineSongIntentSchemaArrangesElectronicSong
  , TestCase "timeline-song-intent-schema-uses-top-level-defaults" testTimelineSongIntentSchemaUsesTopLevelDefaults
  , TestCase "timeline-song-intent-schema-supports-ambient-genre" testTimelineSongIntentSchemaSupportsAmbientGenre
  , TestCase "timeline-song-intent-schema-supports-blackmetal-genre" testTimelineSongIntentSchemaSupportsBlackmetalGenre
  , TestCase "timeline-song-intent-schema-supports-cinematic-genre" testTimelineSongIntentSchemaSupportsCinematicGenre
  , TestCase "timeline-live-genre-switch-updates-future-bars" testTimelineLiveGenreSwitchUpdatesFutureBars
  , TestCase "timeline-harmony-aware-bass-approaches-next-chord" testTimelineHarmonyAwareBassApproachesNextChord
  , TestCase "timeline-melody-anchor-guides-generated-harmony" testTimelineMelodyAnchorGuidesGeneratedHarmony
  , TestCase "timeline-velocity-variation-accents-downbeats" testTimelineVelocityVariationAccentsDownbeats
  , TestCase "timeline-velocity-variation-zero-keeps-base-velocity" testTimelineVelocityVariationZeroKeepsBaseVelocity
  , TestCase "timeline-compiler-builds-cue-sections" testTimelineCompilerBuildsCueSections
  , TestCase "timeline-lookahead-pops-ready-bars" testTimelineLookaheadPopsReadyBars
  , TestCase "timeline-conductor-transitions-at-phrase-boundary" testTimelineConductorTransitionsAtPhraseBoundary
  , TestCase "timeline-pattern-variation-is-deterministic" testTimelinePatternVariationIsDeterministic
  , TestCase "timeline-transition-rules-are-structured" testTimelineTransitionRulesAreStructured
  , TestCase "timeline-cue-without-ending-does-not-stop-early" testTimelineCueWithoutEndingDoesNotStopEarly
  , TestCase "timeline-drum-fills-trigger-at-phrase-boundary" testTimelineDrumFillsTriggerAtPhraseBoundary
  , TestCase "timeline-generated-drum-fills-trigger-at-phrase-boundary" testTimelineGeneratedDrumFillsTriggerAtPhraseBoundary
  , TestCase "timeline-generated-drum-fills-follow-time-signature-and-variation" testTimelineGeneratedDrumFillsFollowTimeSignatureAndVariation
  , TestCase "timeline-generated-drum-fills-follow-preceding-cymbal-context" testTimelineGeneratedDrumFillsFollowPrecedingCymbalContext
  , TestCase "timeline-default-drums-are-present" testTimelineDefaultDrumsArePresent
  , TestCase "timeline-chorus-stability-vs-bridge-variation" testTimelineChorusStabilityVsBridgeVariation
  , TestCase "timeline-energy-target-steers-density" testTimelineEnergyTargetSteersDensity
  , TestCase "timeline-energy-target-steers-conductor" testTimelineEnergyTargetSteersConductor
  , TestCase "timeline-mood-target-steers-transposition" testTimelineMoodTargetSteersTransposition
  , TestCase "timeline-section-metadata-steers-conductor" testTimelineSectionMetadataSteersConductor
  , TestCase "timeline-section-feel-steers-variation" testTimelineSectionFeelSteersVariation
  , TestCase "timeline-transition-telemetry-emits-reason-and-weights" testTimelineTransitionTelemetryEmitsReasonAndWeights
  , TestCase "timeline-runtime-end-frame-includes-last-bar-length" testTimelineRuntimeEndFrameIncludesLastBarLength
  , TestCase "timeline-next-bar-span-follows-tempo-and-meter-changes" testTimelineNextBarSpanFollowsTempoAndMeterChanges
  , TestCase "timeline-drum-timing-humanization-is-deterministic" testTimelineDrumTimingHumanizationIsDeterministic
  , TestCase "automation-energy-lane-ramps-and-completes" testAutomationEnergyLaneRampsAndCompletes
  , TestCase "automation-mood-lane-switches-at-end" testAutomationMoodLaneSwitchesAtEnd
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
  , TestCase "gm-kick-uses-separate-body-and-attack-tuning" testGmKickUsesSeparateBodyAndAttackTuning
  , TestCase "gm-tom-family-has-noise-attack-and-ascending-tuning" testGmTomFamilyHasNoiseAttackAndAscendingTuning
  , TestCase "gm-crash-has-long-noise-tail" testGmCrashHasLongNoiseTail
  , TestCase "gm-program-map-varies-by-family" testGmProgramMapVariesByFamily
  , TestCase "gm-percussion-channel-ignores-program" testGmPercussionChannelIgnoresProgram
  , TestCase "gm-drum-map-varies-by-key-family" testGmDrumMapVariesByKeyFamily
  , TestCase "gm-drum-map-covers-standard-toms-and-cymbals" testGmDrumMapCoversStandardTomsAndCymbals
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

testTimelineSongSpecParsesYaml ∷ IO ()
testTimelineSongSpecParsesYaml = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 3"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    beat_unit: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    mood: tense"
          , "    feel: sparse"
          , "  verse:"
          , "    tempo_bpm: 100"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 2"
          , "instruments:"
          , "  bass:"
          , "    instrument_id: 1"
          , "    amp: 0.9"
          , "    pan: -0.1"
          , "    patterns:"
          , "      intro: 0/36/1/0.8"
          , "      verse: 0/40/0.5/0.7,2/43/0.5/0.7"
          ]
  case parseSongSpecText yamlText of
    Left err ->
      error ("expected timeline song spec to parse, got error: " <> err)
    Right spec -> do
      assertEqual "song mode" SongModeCue (sgMode spec)
      assertEqual "lookahead bars" 3 (sgLookaheadBars spec)
      assertEqual "section count" 2 (M.size (sgSections spec))
      assertEqual "instrument count" 1 (length (sgInstruments spec))

testTimelineSongIntentSchemaArrangesElectronicSong ∷ IO ()
testTimelineSongIntentSchemaArrangesElectronicSong = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "  form: intro,verse,outro"
          , "sections:"
          , "  intro:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Am(add9),Fmaj7"
          , "    melody: 2.0/A4/1.0/0.54,3.0/C5/0.75/0.58,4.0/E5/0.5/0.62"
          , "  verse:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Am,F"
          , "    melody: 0.0/A4/0.5/0.62,4.0/C5/0.5/0.60"
          , "  outro:"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    chords: Am"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      leadKeys bar = [ key | note <- tbNotes bar, tnInstrumentId note == InstrumentId 80, let NoteKey key = tnKey note ]
      hasInstrument iid bar = any ((== iid) . tnInstrumentId) (tbNotes bar)
  assertEqual "generated electronic arrangement should create its instrument palette" 5 (length (sgInstruments spec))
  assertEqual "form should compile into expected bar count" 5 (length bars)
  case bars of
    introBar0 : introBar1 : _ -> do
      assertBool "intro bar should include generated drums" (hasInstrument (InstrumentId 9) introBar0)
      assertBool "intro bar should include generated bass" (hasInstrument (InstrumentId 38) introBar0)
      assertBool "intro bar should include generated pad" (hasInstrument (InstrumentId 88) introBar0)
      assertEqual "phrase-relative melody should stay out of the first bar" [] (filter (== 76) (leadKeys introBar0))
      assertEqual "phrase-relative melody should land in the second bar" [76] (filter (== 76) (leadKeys introBar1))
    _ ->
      error "expected arranged bars for generated electronic song"

testTimelineSongIntentSchemaUsesTopLevelDefaults ∷ IO ()
testTimelineSongIntentSchemaUsesTopLevelDefaults = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  tempo_bpm: 98"
          , "  beats_per_bar: 3"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    chords: Am"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
  case bars of
    bar0 : _ -> do
      assertNear "top-level tempo should flow into sections" 98 (tbTempoBpm bar0)
      assertEqual "top-level beats-per-bar should flow into sections" 3 (tbBeatsPerBar bar0)
      assertBool "generated arrangement should still create notes" (not (null (tbNotes bar0)))
    [] ->
      error "expected at least one compiled bar"

testTimelineSongIntentSchemaSupportsAmbientGenre ∷ IO ()
testTimelineSongIntentSchemaSupportsAmbientGenre = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: ambient"
          , "  mood: calm"
          , "  tempo_bpm: 72"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Am,Fmaj7"
          , "    melody: 0.0/E5/2.0/0.4"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      ambientLead = any (\bar -> any ((== InstrumentId 88) . tnInstrumentId) (tbNotes bar)) bars
      ambientPad = any (\bar -> any ((== InstrumentId 94) . tnInstrumentId) (tbNotes bar)) bars
  assertEqual "ambient genre should be recorded on the song spec" "ambient" (sgGenre spec)
  assertBool "ambient arrangement should use its own lead instrument" ambientLead
  assertBool "ambient arrangement should use its own pad instrument" ambientPad

testTimelineSongIntentSchemaSupportsBlackmetalGenre ∷ IO ()
testTimelineSongIntentSchemaSupportsBlackmetalGenre = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: blackmetal"
          , "  mood: intense"
          , "  tempo_bpm: 160"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Em,C"
          , "    melody: 0.0/E5/0.5/0.7"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      blackmetalLead = any (\bar -> any ((== InstrumentId 31) . tnInstrumentId) (tbNotes bar)) bars
      blackmetalArp = any (\bar -> any ((== InstrumentId 30) . tnInstrumentId) (tbNotes bar)) bars
      blackmetalDrums = any (\bar -> any ((== InstrumentId 9) . tnInstrumentId) (tbNotes bar)) bars
  assertEqual "blackmetal genre should be recorded on the song spec" "blackmetal" (sgGenre spec)
  assertBool "blackmetal arrangement should use a lead guitar program" blackmetalLead
  assertBool "blackmetal arrangement should use tremolo guitar accompaniment" blackmetalArp
  assertBool "blackmetal arrangement should keep drums active" blackmetalDrums

testTimelineSongIntentSchemaSupportsCinematicGenre ∷ IO ()
testTimelineSongIntentSchemaSupportsCinematicGenre = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: cinematic"
          , "  mood: dramatic"
          , "  tempo_bpm: 90"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Dm,Bb"
          , "    melody: 0.0/F4/1.0/0.6,2.0/A4/1.0/0.66"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      leadPrograms = [ iid | bar <- bars, note <- tbNotes bar, let iid = tnInstrumentId note, iid == InstrumentId 61 ]
      padPrograms = [ iid | bar <- bars, note <- tbNotes bar, let iid = tnInstrumentId note, iid == InstrumentId 91 ]
  assertEqual "cinematic genre should be recorded on the song spec" "cinematic" (sgGenre spec)
  assertBool "cinematic arrangement should use its own lead instrument" (not (null leadPrograms))
  assertBool "cinematic arrangement should use its own pad instrument" (not (null padPrograms))

testTimelineLiveGenreSwitchUpdatesFutureBars ∷ IO ()
testTimelineLiveGenreSwitchUpdatesFutureBars = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 1"
          , "    chords: Am,F,C,G"
          , "    melody: 0.0/A4/0.5/0.62,4.0/C5/0.5/0.60,8.0/E5/0.5/0.64,12.0/G5/0.5/0.68"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = (prepareTimelineRuntime testSampleRate 0 spec) { trLookaheadBars = 0 }
      (bars0, runtime1) = popReadyBars 0 runtime0
  case bars0 of
    firstBar : _ -> do
      assertBool "first bar should use electronic lead before switch" (any ((== InstrumentId 80) . tnInstrumentId) (tbNotes firstBar))
      runtime2 <- either (\err -> error ("genre switch failed: " <> err)) pure (setTimelineGenre "cinematic" runtime1)
      let nextNow = tbLengthFrames firstBar
          (bars1, _runtime3) = popReadyBars nextNow runtime2
      case bars1 of
        switchedBar : _ -> do
          assertBool "future bar should use cinematic lead after switch" (any ((== InstrumentId 61) . tnInstrumentId) (tbNotes switchedBar))
          assertBool "future bar should stop using the old electronic lead" (not (any ((== InstrumentId 80) . tnInstrumentId) (tbNotes switchedBar)))
        [] ->
          error "expected a future bar after genre switch"
    [] ->
      error "expected an initial bar before genre switch"

testTimelineHarmonyAwareBassApproachesNextChord ∷ IO ()
testTimelineHarmonyAwareBassApproachesNextChord = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 1"
          , "    chords: Am,F,C,G"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      bassKeys bar =
        [ key
        | note <- tbNotes bar
        , tnInstrumentId note == InstrumentId 38
        , let NoteKey key = tnKey note
        ]
  case bars of
    bar0 : bar1 : _ -> do
      let keys0 = bassKeys bar0
          keys1 = bassKeys bar1
      assertBool "bassline should include more than root-only motion in first bar" (length (nub keys0) > 2)
      assertBool "bassline should include an approach note near the next chord root" (any (`elem` [41, 42, 43]) keys0)
      assertBool "second bar should adapt its bass tones to the F harmony" (any (`elem` [41, 45, 48, 52]) keys1)
    _ ->
      error "expected multiple bars for bass harmony test"

testTimelineMelodyAnchorGuidesGeneratedHarmony ∷ IO ()
testTimelineMelodyAnchorGuidesGeneratedHarmony = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  verse:"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    chords: Am"
          , "    melody: 1.0/B4/1.5/0.70"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      padPitchClasses bar =
        nub
          [ key `mod` 12
          | note <- tbNotes bar
          , tnInstrumentId note == InstrumentId 88
          , let NoteKey key = tnKey note
          ]
      leadKeys bar =
        [ key
        | note <- tbNotes bar
        , tnInstrumentId note == InstrumentId 80
        , let NoteKey key = tnKey note
        ]
  case bars of
    bar0 : _ -> do
      assertEqual "authored melody should stay intact on the lead instrument" [71] (leadKeys bar0)
      assertBool "generated pad should include the melody anchor pitch class" (11 `elem` padPitchClasses bar0)
    [] ->
      error "expected at least one arranged bar"

testTimelineVelocityVariationAccentsDownbeats ∷ IO ()
testTimelineVelocityVariationAccentsDownbeats = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  lead:"
          , "    instrument_id: 0"
          , "    velocity_variation: 1.0"
          , "    pattern_ending: 0/49/0.5/0.5,1/49/0.5/0.5,2/49/0.5/0.5"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
  case bars of
    bar0 : _ -> do
      let notes = tbNotes bar0
          vBeat1 = noteVelocityAtFrame 0 notes
          vBeat2 = noteVelocityAtFrame 24000 notes
          vBeat3 = noteVelocityAtFrame 48000 notes
      assertBool "beat 1 should be loudest under velocity variation" (vBeat1 > vBeat3)
      assertBool "beat 3 should be louder than beat 2 under velocity variation" (vBeat3 > vBeat2)
    [] ->
      error "expected at least one compiled bar"

testTimelineVelocityVariationZeroKeepsBaseVelocity ∷ IO ()
testTimelineVelocityVariationZeroKeepsBaseVelocity = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  lead:"
          , "    instrument_id: 0"
          , "    velocity_variation: 0.0"
          , "    pattern_ending: 0/49/0.5/0.5,1/49/0.5/0.5,2/49/0.5/0.5"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
  case bars of
    bar0 : _ -> do
      let notes = tbNotes bar0
      assertNear "beat 1 base velocity remains unchanged" 0.5 (noteVelocityAtFrame 0 notes)
      assertNear "beat 2 base velocity remains unchanged" 0.5 (noteVelocityAtFrame 24000 notes)
      assertNear "beat 3 base velocity remains unchanged" 0.5 (noteVelocityAtFrame 48000 notes)
    [] ->
      error "expected at least one compiled bar"

testTimelineCompilerBuildsCueSections ∷ IO ()
testTimelineCompilerBuildsCueSections = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  synth:"
          , "    instrument_id: 2"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.9"
          , "    pattern_verse: 0/62/1/0.9"
          , "    pattern_chorus: 0/64/1/0.9"
          , "    pattern_ending: 0/65/1/0.9"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars = compileTimelineBars testSampleRate spec
      sections = map tbSectionName bars
  assertEqual
    "cue section order should repeat verse/chorus deterministically"
    ["intro", "verse", "chorus", "verse", "chorus", "ending"]
    sections
  case bars of
    firstBar : _ -> do
      assertEqual "bar length at 120 bpm / 4 beats" 96000 (tbLengthFrames firstBar)
      case tbNotes firstBar of
        n0 : _ -> do
          assertEqual "note should start at bar offset 0" 0 (tnOnOffsetFrames n0)
          assertEqual "1 beat duration should map to quarter-note frames" 24000 (tnOffOffsetFrames n0)
        [] ->
          error "expected at least one note in first bar"
    [] ->
      error "expected compiled bars"

testTimelineLookaheadPopsReadyBars ∷ IO ()
testTimelineLookaheadPopsReadyBars = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pulse:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/60/0.5/1"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 1000 spec
      (batch1, runtime1) = popReadyBars 0 runtime0
      (batch2, runtime2) = popReadyBars 100000 runtime1
  assertEqual "first lookahead batch should include two bars" 2 (length batch1)
  assertEqual "second lookahead batch should include remaining two bars" 2 (length batch2)
  assertBool "runtime should be done after scheduling all bars" (timelineRuntimeDone runtime2)

testTimelineConductorTransitionsAtPhraseBoundary ∷ IO ()
testTimelineConductorTransitionsAtPhraseBoundary = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 3"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pad:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_verse: 0/62/1/0.8"
          , "    pattern_chorus: 0/64/1/0.8"
          , "    pattern_ending: 0/65/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (bars, runtimeFinal) = drainTimelineBars 0 0 [] runtime0
      sectionRuns = runLengthsBySection bars
  assertBool "conductor should emit at least one bar" (not (null bars))
  case bars of
    [] -> error "expected bars from conductor"
    firstBar : _ -> assertEqual "first section should be intro" "intro" (tbSectionName firstBar)
  assertEqual "last section should resolve to ending in cue mode" "ending" (tbSectionName (last bars))
  assertBool
    "phrase-boundary transitions should preserve even-length section runs"
    (all (even . snd) sectionRuns)
  assertBool "conductor runtime should finish in cue mode" (timelineRuntimeDone runtimeFinal)
  where
    drainTimelineBars ∷ Word64 → Int → [TimelineBar] → TimelineRuntime → ([TimelineBar], TimelineRuntime)
    drainTimelineBars now loops acc rt
      | loops > 200 = error "timeline conductor did not finish within expected iterations"
      | otherwise =
          let (batch, rt') = popReadyBars now rt
              acc' = acc <> batch
          in if timelineRuntimeDone rt'
               then (acc', rt')
               else drainTimelineBars (now + 100000) (loops + 1) acc' rt'

    runLengthsBySection ∷ [TimelineBar] → [(String, Int)]
    runLengthsBySection [] = []
    runLengthsBySection (b : bs) = go (tbSectionName b) 1 bs
      where
        go name n [] = [(name, n)]
        go name n (x : xs)
          | tbSectionName x == name = go name (n + 1) xs
          | otherwise = (name, n) : go (tbSectionName x) 1 xs

testTimelineTransitionTelemetryEmitsReasonAndWeights ∷ IO ()
testTimelineTransitionTelemetryEmitsReasonAndWeights = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 3"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pad:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_ending: 0/62/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 =
        setTimelineTargets (Just "combat") 0.9 (prepareTimelineRuntime testSampleRate 0 spec)
      (_bars, telemetry, runtimeFinal) = drainTimelineWithTelemetry 0 0 [] [] runtime0
  assertBool "telemetry should include one transition event" (length telemetry == 1)
  case telemetry of
    [] -> error "expected transition telemetry event"
    ev : _ -> do
      assertEqual "from section" "intro" (ttFromSectionName ev)
      assertEqual "to section" "ending" (ttToSectionName ev)
      assertEqual "transition reason" "phrase-boundary-weighted" (ttReason ev)
      assertEqual "transition boundary bar index" 0 (ttBoundaryBarIx ev)
      assertBool "transition boundary should be positive frame offset" (ttBoundaryOffsetFrames ev > 0)
      assertEqual "base scripted weights" [("ending", 1)] (ttBaseWeights ev)
      assertBool
        "final weighted candidates should still include ending"
        (any ((== "ending") . fst) (ttFinalWeights ev))
      assertEqual "mood target should be captured" (Just "combat") (ttMoodTarget ev)
      assertBool "energy target should be captured" (abs (ttEnergyTarget ev - 0.9) < 0.0001)
      assertBool "pick total should be positive" (ttPickTotal ev > 0)
      assertBool
        "pick ticket should be within total"
        (ttPickTicket ev >= 0 && ttPickTicket ev < ttPickTotal ev)
  assertBool "runtime should finish in cue mode after ending" (timelineRuntimeDone runtimeFinal)
  where
    drainTimelineWithTelemetry
      ∷ Word64
      → Int
      → [TimelineBar]
      → [TimelineTransitionTelemetry]
      → TimelineRuntime
      → ([TimelineBar], [TimelineTransitionTelemetry], TimelineRuntime)
    drainTimelineWithTelemetry now loops barsAcc telemetryAcc rt
      | loops > 200 = error "timeline telemetry did not finish within expected iterations"
      | otherwise =
          let (batch, rt1) = popReadyBars now rt
              (events, rt2) = popTransitionTelemetry rt1
              barsAcc' = barsAcc <> batch
              telemetryAcc' = telemetryAcc <> events
          in if timelineRuntimeDone rt2
               then (barsAcc', telemetryAcc', rt2)
               else
                  drainTimelineWithTelemetry
                    (now + 100000)
                    (loops + 1)
                    barsAcc'
                    telemetryAcc'
                    rt2

testTimelineRuntimeEndFrameIncludesLastBarLength ∷ IO ()
testTimelineRuntimeEndFrameIncludesLastBarLength = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineTempoChangeYaml)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      runtime1 = driveTimelineUntilDone (0 ∷ Word64) 0 runtime0
  assertBool "runtime should have generated every bar" (timelineRuntimeDone runtime1)
  assertEqual "end frame should include the full last bar" (Just 240000) (timelineRuntimeEndFrame runtime1)
  where
    driveTimelineUntilDone ∷ Word64 → Int → TimelineRuntime → TimelineRuntime
    driveTimelineUntilDone now loops rt
      | loops > 32 = error "timeline runtime did not finish within expected iterations"
      | timelineRuntimeDone rt = rt
      | otherwise =
          let (_readyBars, rt') = popReadyBars now rt
          in driveTimelineUntilDone (now + 100000) (loops + 1) rt'

testTimelineNextBarSpanFollowsTempoAndMeterChanges ∷ IO ()
testTimelineNextBarSpanFollowsTempoAndMeterChanges = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineTempoChangeYaml)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (mSpan, _runtime1) = timelineBoundarySpanFromNextBar 1 1 runtime0
  assertEqual
    "next-bar span should follow the upcoming ending tempo and meter"
    (Just (96000, 240000))
    mSpan

timelineTempoChangeYaml ∷ String
timelineTempoChangeYaml =
  unlines
    [ "song:"
    , "  mode: cue"
    , "sections:"
    , "  intro:"
    , "    tempo_bpm: 120"
    , "    beats_per_bar: 4"
    , "    bars_per_phrase: 1"
    , "    phrase_count: 1"
    , "  ending:"
    , "    tempo_bpm: 60"
    , "    beats_per_bar: 3"
    , "    bars_per_phrase: 1"
    , "    phrase_count: 1"
    , "instruments:"
    , "  bass:"
    , "    instrument_id: 0"
    , "    patterns:"
    , "      intro: 0/36/0.5/0.8"
    , "      ending: 0/40/0.5/0.8"
    ]

testTimelineDrumTimingHumanizationIsDeterministic ∷ IO ()
testTimelineDrumTimingHumanizationIsDeterministic = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  drums:"
          , "    instrument_id: 9"
          , "    velocity_variation: 1"
          , "    patterns:"
          , "      intro: 0/49/0.2/0.4,0.5/49/0.2/0.5,1/49/0.2/0.6,1.5/49/0.2/0.7"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let bars1 = compileTimelineBars testSampleRate spec
      bars2 = compileTimelineBars testSampleRate spec
  case (bars1, bars2) of
    (bar1 : _, bar2 : _) -> do
      let offsets1 = map tnOnOffsetFrames (tbNotes bar1)
          offsets2 = map tnOnOffsetFrames (tbNotes bar2)
          exactOffsets = [0, 12000, 24000, 36000] ∷ [Word64]
      assertEqual "drum timing humanization should be deterministic" offsets1 offsets2
      assertBool "cymbal notes should not all stay perfectly on-grid" (offsets1 /= exactOffsets)
      assertBool
        "timing humanization should stay within a small window"
        (and (zipWith (\actual expected -> abs (fromIntegral actual - fromIntegral expected ∷ Int) <= 720) offsets1 exactOffsets))
    _ ->
      error "expected compiled drum bar"

testTimelineTransitionRulesAreStructured ∷ IO ()
testTimelineTransitionRulesAreStructured = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 2"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 2"
          , "  bridge:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 2"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pulse:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_verse: 0/62/1/0.8"
          , "    pattern_chorus: 0/64/1/0.8"
          , "    pattern_bridge: 0/65/1/0.8"
          , "    pattern_ending: 0/67/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (bars, runtimeFinal) = drainTimelineBarsForTest 0 0 [] runtime0
      names = map tbSectionName bars
      transitions = filter (\(a, b) -> a /= b) (zip names (drop 1 names))
      allowedFollowers prev =
        case prev of
          "intro" -> ["verse"]
          "verse" -> ["chorus", "bridge", "ending"]
          "chorus" -> ["chorus", "bridge", "ending"]
          "bridge" -> ["chorus", "bridge", "ending"]
          "ending" -> []
          _ -> ["chorus", "bridge", "ending"]
      legal (fromS, toS) = toS `elem` allowedFollowers fromS
  assertBool "expected non-empty generated bars" (not (null bars))
  case names of
    a : b : _ -> do
      assertEqual "song should start with intro" "intro" a
      assertEqual "intro must transition to verse" "verse" b
    _ -> error "expected at least two bars"
  assertBool
    "all section transitions should follow constrained transition rules"
    (all legal transitions)
  assertEqual "song should always end on ending section" "ending" (last names)
  assertBool "cue timeline should complete" (timelineRuntimeDone runtimeFinal)

testTimelineCueWithoutEndingDoesNotStopEarly ∷ IO ()
testTimelineCueWithoutEndingDoesNotStopEarly = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pulse:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_verse: 0/62/1/0.8"
          , "    pattern_chorus: 0/64/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      bars = takeTimelineBars 24 0 0 [] runtime0
      hasVerse = any ((== "verse") . tbSectionName) bars
      hasChorus = any ((== "chorus") . tbSectionName) bars
  assertEqual "expected requested number of bars" 24 (length bars)
  assertBool "should continue running without ending section" hasVerse
  assertBool "should still transition within available sections" hasChorus
  where
    takeTimelineBars ∷ Int → Word64 → Int → [TimelineBar] → TimelineRuntime → [TimelineBar]
    takeTimelineBars target now loops acc rt
      | loops > 400 = acc
      | length acc >= target = take target acc
      | otherwise =
          let (batch, rt') = popReadyBars now rt
              acc' = acc <> batch
          in takeTimelineBars target (now + 120000) (loops + 1) acc' rt'

testTimelineDefaultDrumsArePresent ∷ IO ()
testTimelineDefaultDrumsArePresent = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pad:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/60/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (bars, _done) = drainTimelineBarsForTest 0 0 [] runtime0
      hasDrums bar =
        any
          (\n -> case tnInstrumentId n of InstrumentId iid -> iid == 9)
          (tbNotes bar)
  assertBool "expected bars for default drum fallback test" (not (null bars))
  assertBool "every generated bar should include fallback drum notes" (all hasDrums bars)

testTimelineDrumFillsTriggerAtPhraseBoundary ∷ IO ()
testTimelineDrumFillsTriggerAtPhraseBoundary = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 2"
          , "instruments:"
          , "  drums:"
          , "    instrument_id: 9"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/36/0.25/0.9,2/38/0.25/0.9"
          , "    fill_ending: 0/47/0.25/0.8,1/45/0.25/0.8,2/43/0.25/0.8,3/41/0.25/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (bars, _done) = drainTimelineBarsForTest 0 0 [] runtime0
      hasKey key bar =
        any
          (\n ->
             let NoteKey k = tnKey n
             in k == key)
          (tbNotes bar)
      boundaryBars = filter (\b -> tbBarInPhrase b == 3) bars
      nonBoundaryBars = filter (\b -> tbBarInPhrase b /= 3) bars
  assertEqual "expected 8 bars from two 4-bar phrases" 8 (length bars)
  assertEqual "expected two phrase-boundary bars" 2 (length boundaryBars)
  assertBool "boundary bars should use fill tom notes" (all (hasKey 47) boundaryBars)
  assertBool "boundary bars should not keep base kick note when fill is defined" (all (not . hasKey 36) boundaryBars)
  assertBool "non-boundary bars should keep base beat notes" (all (hasKey 36) nonBoundaryBars)
  assertBool "non-boundary bars should not contain fill tom note" (all (not . hasKey 47) nonBoundaryBars)

testTimelineGeneratedDrumFillsTriggerAtPhraseBoundary ∷ IO ()
testTimelineGeneratedDrumFillsTriggerAtPhraseBoundary = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 4"
          , "    phrase_count: 2"
          , "    mood: intense"
          , "    feel: driving"
          , "instruments:"
          , "  drums:"
          , "    instrument_id: 9"
          , "    amp: 1"
          , "    pan: 0"
          , "    velocity_variation: 0"
          , "    pattern_ending: 0/36/0.25/0.90,1/42/0.12/0.46,2/38/0.22/0.84,3/42/0.12/0.48"
          , "    fill_generator:"
          , "      enabled: true"
          , "      variation: 0.72"
          , "      density: 0.76"
          , "      length_beats: 2.0"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtimeA0 = prepareTimelineRuntime testSampleRate 0 spec
      runtimeB0 = prepareTimelineRuntime testSampleRate 0 spec
      (barsA, _doneA) = drainTimelineBarsForTest 0 0 [] runtimeA0
      (barsB, _doneB) = drainTimelineBarsForTest 0 0 [] runtimeB0
      boundaryBars = filter (\b -> tbBarInPhrase b == 3) barsA
      nonBoundaryBars = filter (\b -> tbBarInPhrase b /= 3) barsA
      hasFillVoice bar =
        any
          (\n ->
             let NoteKey k = tnKey n
             in k `elem` [45, 47, 48, 50, 49, 52, 55, 57])
          (tbNotes bar)
  assertEqual "generated fills should be deterministic" barsA barsB
  assertEqual "expected two phrase-boundary bars" 2 (length boundaryBars)
  assertBool "generated fill bars should introduce tom or cymbal accents" (all hasFillVoice boundaryBars)
  assertBool "non-boundary bars should stay on the base groove" (all (not . hasFillVoice) nonBoundaryBars)

testTimelineGeneratedDrumFillsFollowTimeSignatureAndVariation ∷ IO ()
testTimelineGeneratedDrumFillsFollowTimeSignatureAndVariation = do
  let mkSpec ∷ Float → Float → Either String SongSpec
      mkSpec variation density =
        parseSongSpecText $
          unlines
            [ "song:"
            , "  mode: cue"
            , "  lookahead_bars: 2"
            , "sections:"
            , "  ending:"
            , "    tempo_bpm: 108"
            , "    beats_per_bar: 3"
            , "    bars_per_phrase: 2"
            , "    phrase_count: 2"
            , "    mood: steady"
            , "    feel: sparse"
            , "instruments:"
            , "  drums:"
            , "    instrument_id: 9"
            , "    amp: 1"
            , "    pan: 0"
            , "    velocity_variation: 0"
            , "    pattern_ending: 0/36/0.25/0.88,1/38/0.22/0.82,2/46/0.24/0.42"
            , "    fill_generator:"
            , "      enabled: true"
            , "      variation: " <> show variation
            , "      density: " <> show density
            , "      length_beats: 1.5"
            ]
  specLow <- either (\err -> error ("parse failed: " <> err)) pure (mkSpec 0.15 0.42)
  specHigh <- either (\err -> error ("parse failed: " <> err)) pure (mkSpec 0.92 0.86)
  let runtimeLow0 = prepareTimelineRuntime testSampleRate 0 specLow
      runtimeHigh0 = prepareTimelineRuntime testSampleRate 0 specHigh
      (barsLow, _doneLow) = drainTimelineBarsForTest 0 0 [] runtimeLow0
      (barsHigh, _doneHigh) = drainTimelineBarsForTest 0 0 [] runtimeHigh0
      boundaryLow = findBoundaryBar 1 barsLow
      boundaryHigh = findBoundaryBar 1 barsHigh
      beatOffsets bar =
        let framesPerBeat =
              (fromIntegral testSampleRate ∷ Float)
                * 60
                / tbTempoBpm bar
        in [ fromIntegral (tnOnOffsetFrames note) / framesPerBeat
           | note <- tbNotes bar
           ]
      lowOffsets = beatOffsets boundaryLow
      highOffsets = beatOffsets boundaryHigh
  assertBool "generated fill in 3/4 should start in the last half of the bar" (all (>= 1.49) lowOffsets && all (>= 1.49) highOffsets)
  assertBool "higher variation should produce denser generated fills" (length (tbNotes boundaryHigh) > length (tbNotes boundaryLow))

testTimelineGeneratedDrumFillsFollowPrecedingCymbalContext ∷ IO ()
testTimelineGeneratedDrumFillsFollowPrecedingCymbalContext = do
  let mkSpec ∷ Int → Either String SongSpec
      mkSpec cymbalKey =
        parseSongSpecText $
          unlines
            [ "song:"
            , "  mode: cue"
            , "  lookahead_bars: 2"
            , "sections:"
            , "  ending:"
            , "    tempo_bpm: 120"
            , "    beats_per_bar: 4"
            , "    bars_per_phrase: 2"
            , "    phrase_count: 1"
            , "    mood: rising"
            , "    feel: driving pulse"
            , "instruments:"
            , "  drums:"
            , "    instrument_id: 9"
            , "    amp: 1"
            , "    pan: 0"
            , "    velocity_variation: 0"
            , "    pattern_ending: 0/36/0.25/0.88,1/" <> show cymbalKey <> "/0.22/0.46,2/38/0.22/0.84,3/" <> show cymbalKey <> "/0.22/0.46"
            , "    fill_generator:"
            , "      enabled: true"
            , "      variation: 0.70"
            , "      density: 0.70"
            , "      length_beats: 2.0"
            ]
      earliestCymbalKeys bar =
        let framesPerBeat =
              (fromIntegral testSampleRate ∷ Float)
                * 60
                / tbTempoBpm bar
            cymbalNotes =
              [ (fromIntegral (tnOnOffsetFrames note) / framesPerBeat, key)
              | note <- tbNotes bar
              , let NoteKey key = tnKey note
              , key `elem` [42, 44, 46, 49, 51, 52, 53, 55, 57, 59]
              ]
        in case cymbalNotes of
             [] -> []
             xs ->
               let firstBeat = minimum (map fst xs)
               in [ key | (beat, key) <- xs, abs (beat - firstBeat) < 0.001 ]
  specRide <- either (\err -> error ("parse failed: " <> err)) pure (mkSpec 51)
  specHat <- either (\err -> error ("parse failed: " <> err)) pure (mkSpec 46)
  let runtimeRide0 = prepareTimelineRuntime testSampleRate 0 specRide
      runtimeHat0 = prepareTimelineRuntime testSampleRate 0 specHat
      (barsRide, _doneRide) = drainTimelineBarsForTest 0 0 [] runtimeRide0
      (barsHat, _doneHat) = drainTimelineBarsForTest 0 0 [] runtimeHat0
      rideBoundary = findBoundaryBar 1 barsRide
      hatBoundary = findBoundaryBar 1 barsHat
  assertBool "ride groove should carry ride color into the generated fill lead-in" (51 `elem` earliestCymbalKeys rideBoundary)
  assertBool "hat groove should carry hat color into the generated fill lead-in" (46 `elem` earliestCymbalKeys hatBoundary)

testTimelineChorusStabilityVsBridgeVariation ∷ IO ()
testTimelineChorusStabilityVsBridgeVariation = do
  let mkSpec sectionName =
        parseSongSpecText $
          unlines
            [ "song:"
            , "  mode: cue"
            , "  lookahead_bars: 4"
            , "sections:"
            , "  " <> sectionName <> ":"
            , "    tempo_bpm: 120"
            , "    beats_per_bar: 4"
            , "    bars_per_phrase: 12"
            , "    phrase_count: 1"
            , "instruments:"
            , "  lead:"
            , "    instrument_id: 0"
            , "    amp: 1"
            , "    pan: 0"
            , "    pattern_" <> sectionName <> ": 0/60/0.5/0.9,1/62/0.5/0.9,2/64/0.5/0.9,3/65/0.5/0.9"
            ]
  chorusSpec <- either (\err -> error ("chorus parse failed: " <> err)) pure (mkSpec "chorus")
  bridgeSpec <- either (\err -> error ("bridge parse failed: " <> err)) pure (mkSpec "bridge")
  let chorusBars = takeTimelineBars 24 0 0 [] (prepareTimelineRuntime testSampleRate 0 chorusSpec)
      bridgeBars = takeTimelineBars 24 0 0 [] (prepareTimelineRuntime testSampleRate 0 bridgeSpec)
      melodicSignature bar =
        [ (k, velBucket)
        | n <- tbNotes bar
        , let isLead =
                case tnInstrumentId n of
                  InstrumentId iid -> iid == 0
        , isLead
        , let NoteKey k = tnKey n
        , let velBucket = floor (tnVelocity n * 100) :: Int
        ]
      uniqueCount xs = length (nub xs)
      chorusVariety = uniqueCount (map melodicSignature chorusBars)
      bridgeVariety = uniqueCount (map melodicSignature bridgeBars)
  assertBool "expected chorus bars" (not (null chorusBars))
  assertBool "expected bridge bars" (not (null bridgeBars))
  assertBool
    "bridge sections should vary more than chorus sections"
    (bridgeVariety > chorusVariety)
  where
    takeTimelineBars ∷ Int → Word64 → Int → [TimelineBar] → TimelineRuntime → [TimelineBar]
    takeTimelineBars target now loops acc rt
      | loops > 400 = acc
      | length acc >= target = take target acc
      | otherwise =
          let (batch, rt') = popReadyBars now rt
              acc' = acc <> batch
          in takeTimelineBars target (now + 120000) (loops + 1) acc' rt'

testTimelinePatternVariationIsDeterministic ∷ IO ()
testTimelinePatternVariationIsDeterministic = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 4"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 8"
          , "    phrase_count: 1"
          , "instruments:"
          , "  rhythm:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/60/0.5/0.9,1/62/0.5/0.9,2/64/0.5/0.9,3/65/0.5/0.9"
          , "  drone:"
          , "    instrument_id: 1"
          , "    amp: 0.8"
          , "    pan: -0.2"
          , "    pattern_ending: 0/48/2/0.7,2/50/2/0.7"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let baselineBars = compileTimelineBars testSampleRate spec
      runtimeA0 = prepareTimelineRuntime testSampleRate 0 spec
      runtimeB0 = prepareTimelineRuntime testSampleRate 0 spec
      runtimeC0 = prepareTimelineRuntime testSampleRate 1337 spec
      (barsA, doneA) = drainTimelineBarsForTest 0 0 [] runtimeA0
      (barsB, _doneB) = drainTimelineBarsForTest 0 0 [] runtimeB0
      (barsC, _doneC) = drainTimelineBarsForTest 0 0 [] runtimeC0
  assertBool "runtime should finish after intro section" (timelineRuntimeDone doneA)
  assertEqual "deterministic seed should produce identical mutated bars" barsA barsB
  assertBool "different seed should alter mutated bars" (barsA /= barsC)
  case (baselineBars, barsA) of
    (base0 : _, runtime0 : _) -> do
      assertBool
        "variation layer should mutate note stream vs baseline compiler"
        (tbNotes runtime0 /= tbNotes base0 || any (\b -> tbNotes b /= tbNotes base0) barsA)
    _ ->
      error "expected non-empty baseline/runtime bars for variation test"

testTimelineEnergyTargetSteersDensity ∷ IO ()
testTimelineEnergyTargetSteersDensity = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 4"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 12"
          , "    phrase_count: 1"
          , "instruments:"
          , "  rhythm:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/60/0.25/0.9,0.5/62/0.25/0.9,1/64/0.25/0.9,1.5/65/0.25/0.9,2/67/0.25/0.9,2.5/69/0.25/0.9,3/71/0.25/0.9,3.5/72/0.25/0.9"
          , "  drone:"
          , "    instrument_id: 1"
          , "    amp: 0.7"
          , "    pan: -0.1"
          , "    pattern_ending: 0/48/2/0.6,2/50/2/0.6"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtimeLow0 = setTimelineTargets Nothing 0.1 (prepareTimelineRuntime testSampleRate 0 spec)
      runtimeHigh0 = setTimelineTargets Nothing 0.9 (prepareTimelineRuntime testSampleRate 0 spec)
      (barsLow, _doneLow) = drainTimelineBarsForTest 0 0 [] runtimeLow0
      (barsHigh, _doneHigh) = drainTimelineBarsForTest 0 0 [] runtimeHigh0
      noteCount bars = sum (map (length . tbNotes) bars)
  assertBool
    "higher energy target should increase total scheduled note density"
    (noteCount barsHigh > noteCount barsLow)

testTimelineEnergyTargetSteersConductor ∷ IO ()
testTimelineEnergyTargetSteersConductor = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: drone"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "  bridge:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pulse:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_verse: 0/62/1/0.8"
          , "    pattern_chorus: 0/64/1/0.8"
          , "    pattern_bridge: 0/65/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtimeLow0 = setTimelineTargets Nothing 0.1 (prepareTimelineRuntime testSampleRate 0 spec)
      runtimeHigh0 = setTimelineTargets Nothing 0.9 (prepareTimelineRuntime testSampleRate 0 spec)
      barsLow = takeTimelineBars 48 0 0 [] runtimeLow0
      barsHigh = takeTimelineBars 48 0 0 [] runtimeHigh0
  assertBool
    "high energy target should bias conductor to higher-energy sections"
    (avgEnergy barsHigh > avgEnergy barsLow + 0.03)
  where
    avgEnergy ∷ [TimelineBar] → Float
    avgEnergy bars =
      let xs = map (sectionEnergy . tbSectionName) (drop 1 bars)
      in if null xs then 0 else sum xs / fromIntegral (length xs)

    sectionEnergy name =
      case name of
        "intro" -> 0.2
        "verse" -> 0.55
        "chorus" -> 0.9
        "bridge" -> 0.4
        "ending" -> 0.1
        _ -> 0.5

    takeTimelineBars ∷ Int → Word64 → Int → [TimelineBar] → TimelineRuntime → [TimelineBar]
    takeTimelineBars target now loops acc rt
      | loops > 400 = acc
      | length acc >= target = take target acc
      | otherwise =
          let (batch, rt') = popReadyBars now rt
              acc' = acc <> batch
          in takeTimelineBars target (now + 120000) (loops + 1) acc' rt'

testTimelineMoodTargetSteersTransposition ∷ IO ()
testTimelineMoodTargetSteersTransposition = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "  lookahead_bars: 4"
          , "sections:"
          , "  ending:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 8"
          , "    phrase_count: 1"
          , "instruments:"
          , "  lead:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_ending: 0/60/1/0.8,1/62/1/0.8,2/64/1/0.8,3/65/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtimeCalm0 = setTimelineTargets (Just "ambient") 0.5 (prepareTimelineRuntime testSampleRate 0 spec)
      runtimeCombat0 = setTimelineTargets (Just "combat") 0.5 (prepareTimelineRuntime testSampleRate 0 spec)
      (barsCalm, _doneCalm) = drainTimelineBarsForTest 0 0 [] runtimeCalm0
      (barsCombat, _doneCombat) = drainTimelineBarsForTest 0 0 [] runtimeCombat0
      avgPitch bars =
        let keys =
              [ fromIntegral k ∷ Float
              | b <- bars
              , n <- tbNotes b
              , let NoteKey k = tnKey n
              ]
        in if null keys then 0 else sum keys / fromIntegral (length keys)
  assertBool
    "combat mood target should bias transposition upward vs ambient"
    (avgPitch barsCombat > avgPitch barsCalm + 0.5)

testAutomationEnergyLaneRampsAndCompletes ∷ IO ()
testAutomationEnergyLaneRampsAndCompletes = do
  let now = 12345
      sampleRateHz = testSampleRate
      beatsPerBar = 4
      bpm = 120
      lane0 =
        scheduleEnergyLaneNextBar
          now
          sampleRateHz
          beatsPerBar
          bpm
          0.2
          0.8
          2
          AutomationLinear
      barFrames = barFramesForTransport sampleRateHz beatsPerBar bpm
      startFrame = ((now `div` barFrames) + 1) * barFrames
      beforeFrame = max 0 (startFrame - 1)
      midFrame = startFrame + barFrames
      endFrame = startFrame + 2 * barFrames

      (v0, mLane1, ev0) = stepEnergyLane beforeFrame lane0
  assertEqual "before start has no target update" Nothing v0
  assertEqual "before start has no events" [] ev0

  lane1 <- maybe (error "expected lane before start") pure mLane1
  let (v1, mLane2, ev1) = stepEnergyLane startFrame lane1
  assertEqual "start frame emits initial value" (Just 0.2) v1
  assertEqual
    "start frame emits started event"
    [EnergyLaneStarted startFrame endFrame 0.2 0.8 AutomationLinear]
    ev1

  lane2 <- maybe (error "expected lane at start") pure mLane2
  let (v2, mLane3, ev2) = stepEnergyLane midFrame lane2
  assertBool "midpoint should ramp up" (maybe False (\x -> x > 0.49 && x < 0.51) v2)
  assertEqual "midpoint should not emit more events" [] ev2

  lane3 <- maybe (error "expected lane at midpoint") pure mLane3
  let (v3, mLane4, ev3) = stepEnergyLane endFrame lane3
  assertEqual "end frame reaches final target" (Just 0.8) v3
  assertEqual
    "end frame emits completion event"
    [EnergyLaneCompleted endFrame 0.8]
    ev3
  assertEqual "lane should complete and clear" Nothing mLane4

testAutomationMoodLaneSwitchesAtEnd ∷ IO ()
testAutomationMoodLaneSwitchesAtEnd = do
  let now = 5000
      sampleRateHz = testSampleRate
      beatsPerBar = 4
      bpm = 120
      lane0 =
        scheduleMoodLaneNextBar
          now
          sampleRateHz
          beatsPerBar
          bpm
          (Just "ambient")
          (Just "combat")
          3
      barFrames = barFramesForTransport sampleRateHz beatsPerBar bpm
      startFrame = ((now `div` barFrames) + 1) * barFrames
      endFrame = startFrame + 3 * barFrames
      beforeFrame = max 0 (startFrame - 1)

      (m0, mLane1, ev0) = stepMoodLane beforeFrame lane0
  assertEqual "before start should not update mood" Nothing m0
  assertEqual "before start has no mood events" [] ev0

  lane1 <- maybe (error "expected mood lane before start") pure mLane1
  let (m1, mLane2, ev1) = stepMoodLane startFrame lane1
  assertEqual "start frame should not switch mood yet" Nothing m1
  assertEqual
    "start frame emits started event"
    [MoodLaneStarted startFrame endFrame (Just "ambient") (Just "combat")]
    ev1

  lane2 <- maybe (error "expected mood lane after start") pure mLane2
  let (m2, mLane3, ev2) = stepMoodLane (endFrame - 1) lane2
  assertEqual "mood should remain unchanged before end" Nothing m2
  assertEqual "no extra event before end" [] ev2

  lane3 <- maybe (error "expected mood lane before end") pure mLane3
  let (m3, mLane4, ev3) = stepMoodLane endFrame lane3
  assertEqual "end frame should switch to target mood" (Just (Just "combat")) m3
  assertEqual
    "end frame emits mood completion"
    [MoodLaneCompleted endFrame (Just "combat")]
    ev3
  assertEqual "mood lane should complete and clear" Nothing mLane4

testTimelineSectionMetadataSteersConductor ∷ IO ()
testTimelineSectionMetadataSteersConductor = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: drone"
          , "  lookahead_bars: 2"
          , "sections:"
          , "  intro:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    mood: neutral"
          , "    feel: plain"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    mood: neutral"
          , "    feel: plain"
          , "  chorus:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    mood: aggressive"
          , "    feel: dense driving"
          , "  bridge:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    mood: ambient"
          , "    feel: sparse slow"
          , "instruments:"
          , "  pulse:"
          , "    instrument_id: 0"
          , "    amp: 1"
          , "    pan: 0"
          , "    pattern_intro: 0/60/1/0.8"
          , "    pattern_verse: 0/62/1/0.8"
          , "    pattern_chorus: 0/67/1/0.8"
          , "    pattern_bridge: 0/64/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  let runtimeCalm0 = setTimelineTargets (Just "calm") 0.4 (prepareTimelineRuntime testSampleRate 0 spec)
      runtimeCombat0 = setTimelineTargets (Just "combat") 0.7 (prepareTimelineRuntime testSampleRate 0 spec)
      barsCalm = takeTimelineBars 32 0 0 [] runtimeCalm0
      barsCombat = takeTimelineBars 32 0 0 [] runtimeCombat0
      chorusCount xs = length (filter ((== "chorus") . tbSectionName) xs)
      bridgeCount xs = length (filter ((== "bridge") . tbSectionName) xs)
  assertBool "calm target should prefer ambient metadata sections" (bridgeCount barsCalm > chorusCount barsCalm)
  assertBool "combat target should prefer aggressive metadata sections" (chorusCount barsCombat > bridgeCount barsCombat)
  where
    takeTimelineBars ∷ Int → Word64 → Int → [TimelineBar] → TimelineRuntime → [TimelineBar]
    takeTimelineBars target now loops acc rt
      | loops > 400 = acc
      | length acc >= target = take target acc
      | otherwise =
          let (batch, rt') = popReadyBars now rt
              acc' = acc <> batch
          in takeTimelineBars target (now + 120000) (loops + 1) acc' rt'

testTimelineSectionFeelSteersVariation ∷ IO ()
testTimelineSectionFeelSteersVariation = do
  let mkSpec feelLabel =
        parseSongSpecText $
          unlines
            [ "song:"
            , "  mode: cue"
            , "  lookahead_bars: 4"
            , "sections:"
            , "  ending:"
            , "    tempo_bpm: 120"
            , "    beats_per_bar: 4"
            , "    bars_per_phrase: 10"
            , "    phrase_count: 1"
            , "    mood: neutral"
            , "    feel: " <> feelLabel
            , "instruments:"
            , "  rhythm:"
            , "    instrument_id: 0"
            , "    amp: 1"
            , "    pan: 0"
            , "    pattern_ending: 0/60/0.25/0.9,0.5/62/0.25/0.9,1/64/0.25/0.9,1.5/65/0.25/0.9,2/67/0.25/0.9,2.5/69/0.25/0.9,3/71/0.25/0.9,3.5/72/0.25/0.9"
            ]
  sparseSpec <- either (\err -> error ("parse sparse spec failed: " <> err)) pure (mkSpec "sparse ambient")
  denseSpec <- either (\err -> error ("parse dense spec failed: " <> err)) pure (mkSpec "dense driving")
  let runtimeSparse0 = setTimelineTargets Nothing 0.5 (prepareTimelineRuntime testSampleRate 0 sparseSpec)
      runtimeDense0 = setTimelineTargets Nothing 0.5 (prepareTimelineRuntime testSampleRate 0 denseSpec)
      (barsSparse, _doneSparse) = drainTimelineBarsForTest 0 0 [] runtimeSparse0
      (barsDense, _doneDense) = drainTimelineBarsForTest 0 0 [] runtimeDense0
      noteCount xs = sum (map (length . tbNotes) xs)
  assertBool "dense section feel should yield more notes than sparse feel" (noteCount barsDense > noteCount barsSparse)

drainTimelineBarsForTest ∷ Word64 → Int → [TimelineBar] → TimelineRuntime → ([TimelineBar], TimelineRuntime)
drainTimelineBarsForTest now loops acc rt
  | loops > 400 = error "timeline runtime did not finish within expected iterations"
  | otherwise =
      let (batch, rt') = popReadyBars now rt
          acc' = acc <> batch
      in if timelineRuntimeDone rt'
           then (acc', rt')
           else drainTimelineBarsForTest (now + 120000) (loops + 1) acc' rt'

findBoundaryBar ∷ Int → [TimelineBar] → TimelineBar
findBoundaryBar barInPhrase bars =
  case filter (\bar -> tbBarInPhrase bar == barInPhrase) bars of
    bar : _ -> bar
    [] -> error ("expected bar at phrase slot " <> show barInPhrase)

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

noteVelocityAtFrame ∷ Word64 → [TimelineNote] → Float
noteVelocityAtFrame frame notes =
  case find (\n -> tnOnOffsetFrames n == frame) notes of
    Just n -> tnVelocity n
    Nothing -> error ("missing note at frame " <> show frame)

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
