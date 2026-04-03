{-# LANGUAGE Strict, UnicodeSyntax #-}

module Tests.Timeline (testCases) where

import Data.Foldable (toList)
import Data.List (find, intercalate, isInfixOf, nub)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openBinaryTempFile, hClose)

import Audio.Types (InstrumentId(..), NoteKey(..))
import Perf.Baseline
  ( BaselineScenario(..)
  , BaselineScenarioAction(..)
  , BaselineScenarioStep(..)
  , LookaheadPerfSummary(..)
  , ScheduledBaselineAction(..)
  , lookaheadAverageBarsPerSecond
  , lookaheadAverageGenerationUs
  , lookupBaselineScenario
  , scheduleBaselineScenario
  , summarizeLookaheadTelemetry
  , timelineAbsoluteBarStartFrames
  , timelineAbsoluteEndFrameForBars
  )
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
import Player.Timeline.Explain
  ( TimelineExplainEvent(..)
  , barInstrumentNoteCounts
  , drainTimelineExplainEvents
  )
import Player.Timeline
  ( InstrumentPatternSpec(..)
  , SongSpec
  , SongMode(..)
  , stampLookaheadPerfMetrics
  , TimelineBar(..)
  , TimelineLookaheadTelemetry(..)
  , TimelineNote(..)
  , TimelineRetargetTelemetry(..)
  , TimelineRuntime(..)
  , TimelineTransitionTelemetry(..)
  , compileTimelineBars
  , generatedInstrumentSpecs
  , loadSongSpec
  , lookupGeneratedInstrument
  , parseSongSpecText
  , popLookaheadTelemetry
  , popReadyBars
  , popRetargetTelemetry
  , popTransitionTelemetry
  , prepareTimelineRuntime
  , setTimelineGenre
  , setTimelineTargets
  , sgGenre
  , sgInstruments
  , sgLookaheadBars
  , sgMode
  , sgSections
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  )
import TestSupport
  ( TestCase(..)
  , assertBool
  , assertEqual
  , assertNear
  , catchIgnore
  , noteVelocityAtFrame
  , testSampleRate
  )

testCases ∷ [TestCase]
testCases =
  [ TestCase "timeline-explicit-instrument-patch-path-parses" testTimelineExplicitInstrumentPatchPathParses
  , TestCase "timeline-load-song-spec-reports-missing-file" testTimelineLoadSongSpecReportsMissingFile
  , TestCase "timeline-generated-patch-overrides-survive-genre-switch" testTimelineGeneratedPatchOverridesSurviveGenreSwitch
  , TestCase "timeline-load-song-spec-resolves-patch-paths" testTimelineLoadSongSpecResolvesPatchPaths
  , TestCase "timeline-song-spec-parses-yaml" testTimelineSongSpecParsesYaml
  , TestCase "timeline-song-intent-schema-arranges-electronic-song" testTimelineSongIntentSchemaArrangesElectronicSong
  , TestCase "timeline-song-intent-schema-uses-top-level-defaults" testTimelineSongIntentSchemaUsesTopLevelDefaults
  , TestCase "timeline-chord-regions-override-legacy-chords" testTimelineChordRegionsOverrideLegacyChords
  , TestCase "timeline-song-intent-schema-supports-ambient-genre" testTimelineSongIntentSchemaSupportsAmbientGenre
  , TestCase "timeline-song-intent-schema-supports-blackmetal-genre" testTimelineSongIntentSchemaSupportsBlackmetalGenre
  , TestCase "timeline-song-intent-schema-supports-cinematic-genre" testTimelineSongIntentSchemaSupportsCinematicGenre
  , TestCase "timeline-generated-instrument-lookup-follows-genre" testTimelineGeneratedInstrumentLookupFollowsGenre
  , TestCase "timeline-generated-instrument-lookup-rejects-unknown-layer" testTimelineGeneratedInstrumentLookupRejectsUnknownLayer
  , TestCase "timeline-live-genre-switch-updates-future-bars" testTimelineLiveGenreSwitchUpdatesFutureBars
  , TestCase "timeline-explain-trace-emits-lookahead-and-bar-events" testTimelineExplainTraceEmitsLookaheadAndBarEvents
  , TestCase "timeline-explain-trace-emits-transition-weights" testTimelineExplainTraceEmitsTransitionWeights
  , TestCase "timeline-rapid-adaptive-changes-stay-deterministic" testTimelineRapidAdaptiveChangesStayDeterministic
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
  , TestCase "timeline-retarget-telemetry-reports-future-generation-policy" testTimelineRetargetTelemetryReportsFutureGenerationPolicy
  , TestCase "timeline-retarget-does-not-rewrite-buffered-bars" testTimelineRetargetDoesNotRewriteBufferedBars
  , TestCase "timeline-transition-telemetry-emits-reason-and-weights" testTimelineTransitionTelemetryEmitsReasonAndWeights
  , TestCase "timeline-lookahead-telemetry-reports-boundary-queries" testTimelineLookaheadTelemetryReportsBoundaryQueries
  , TestCase "timeline-lookahead-perf-metrics-compute-throughput" testTimelineLookaheadPerfMetricsComputeThroughput
  , TestCase "timeline-baseline-summary-aggregates-lookahead-metrics" testTimelineBaselineSummaryAggregatesLookaheadMetrics
  , TestCase "timeline-baseline-scenario-lookup-is-case-insensitive" testTimelineBaselineScenarioLookupIsCaseInsensitive
  , TestCase "timeline-baseline-bar-starts-follow-constant-meter" testTimelineBaselineBarStartsFollowConstantMeter
  , TestCase "timeline-baseline-scenario-scheduling-follows-bar-starts" testTimelineBaselineScenarioSchedulingFollowsBarStarts
  , TestCase "timeline-baseline-end-frame-follows-requested-bars" testTimelineBaselineEndFrameFollowsRequestedBars
  , TestCase "timeline-runtime-end-frame-includes-last-bar-length" testTimelineRuntimeEndFrameIncludesLastBarLength
  , TestCase "timeline-next-bar-span-follows-tempo-and-meter-changes" testTimelineNextBarSpanFollowsTempoAndMeterChanges
  , TestCase "timeline-drum-timing-humanization-is-deterministic" testTimelineDrumTimingHumanizationIsDeterministic
  , TestCase "automation-energy-lane-ramps-and-completes" testAutomationEnergyLaneRampsAndCompletes
  , TestCase "automation-mood-lane-switches-at-end" testAutomationMoodLaneSwitchesAtEnd
  ]

testTimelineExplicitInstrumentPatchPathParses ∷ IO ()
testTimelineExplicitInstrumentPatchPathParses = do
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pad:"
          , "    instrument_id: 88"
          , "    patch: config/patches/warm-pad.yaml"
          , "    patterns:"
          , "      verse: 0/C4/1.0/0.8"
          ]
  case parseSongSpecText yamlText of
    Left err ->
      error ("expected explicit patch path parse success, got " <> err)
    Right spec ->
      case find ((== "pad") . ipName) (sgInstruments spec) of
        Nothing ->
          error "expected explicit instrument spec"
        Just inst ->
          assertEqual "explicit instrument should keep its patch path" (Just "config/patches/warm-pad.yaml") (ipPatchPath inst)

testTimelineGeneratedPatchOverridesSurviveGenreSwitch ∷ IO ()
testTimelineGeneratedPatchOverridesSurviveGenreSwitch = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "  patches:"
          , "    pad: config/patches/warm-pad.yaml"
          , "sections:"
          , "  intro:"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "    chords: Am"
          ]
  case parseSongSpecText yamlText of
    Left err ->
      error ("expected generated patch override parse success, got " <> err)
    Right spec -> do
      let expectPadPatch instruments label =
            case find ((== "pad") . ipName) instruments of
              Nothing -> error ("expected pad instrument in " <> label)
              Just inst -> assertEqual (label <> " pad patch override") (Just "config/patches/warm-pad.yaml") (ipPatchPath inst)
      expectPadPatch (sgInstruments spec) "initial generated palette"
      case setTimelineGenre "ambient" (prepareTimelineRuntime 48000 0 spec) of
        Left err ->
          error ("expected genre switch success, got " <> err)
        Right runtime ->
          expectPadPatch (trInstruments runtime) "genre-switched generated palette"

testTimelineLoadSongSpecResolvesPatchPaths ∷ IO ()
testTimelineLoadSongSpecResolvesPatchPaths = do
  (path, handle) <- openBinaryTempFile "." "idou-song-patch.yaml"
  hClose handle
  let yamlText =
        unlines
          [ "song:"
          , "  mode: cue"
          , "sections:"
          , "  verse:"
          , "    tempo_bpm: 120"
          , "    beats_per_bar: 4"
          , "    bars_per_phrase: 1"
          , "    phrase_count: 1"
          , "instruments:"
          , "  pad:"
          , "    instrument_id: 88"
          , "    patch: config/patches/warm-pad.yaml"
          , "    patterns:"
          , "      verse: 0/C4/1.0/0.8"
          ]
  Prelude.writeFile path yamlText
  loaded <- loadSongSpec path
  removeFile path
  case loaded of
    Left err ->
      error ("expected loadSongSpec success, got " <> err)
    Right spec ->
      case find ((== "pad") . ipName) (sgInstruments spec) of
        Nothing ->
          error "expected explicit instrument spec after loading song"
        Just inst ->
          assertEqual "loadSongSpec should resolve relative patch paths" (Just "config/patches/warm-pad.yaml") (ipPatchPath inst)

testTimelineLoadSongSpecReportsMissingFile ∷ IO ()
testTimelineLoadSongSpecReportsMissingFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir <> "/idou-missing-song-spec.yaml"
  catchIgnore (removeFile path)
  loaded <- loadSongSpec path
  case loaded of
    Left err ->
      assertBool "expected song spec read error" ("Failed to read song spec" `isInfixOf` err)
    Right spec ->
      error ("expected missing song spec failure, got " <> show spec)

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

testTimelineChordRegionsOverrideLegacyChords ∷ IO ()
testTimelineChordRegionsOverrideLegacyChords = do
  let yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 2"
          , "    phrase_count: 1"
          , "    chords: Am,F"
          , "    chord_regions:"
          , "      change_1:"
          , "        start_beat: 0"
          , "        symbol: Dm"
          , "      change_2:"
          , "        start_beat: 4"
          , "        symbol: G"
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
    _bar0 : bar1 : _ ->
      assertBool "second bar should follow chord_regions instead of legacy chords" (any (`elem` [43, 47, 50, 55]) (bassKeys bar1))
    _ ->
      error "expected arranged bars for chord region override test"

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

testTimelineGeneratedInstrumentLookupFollowsGenre ∷ IO ()
testTimelineGeneratedInstrumentLookupFollowsGenre = do
  ambient <- either (\err -> error ("expected ambient palette lookup to succeed, got " <> err)) pure $
    generatedInstrumentSpecs "ambient"
  assertEqual "ambient generated palette should keep five layers" 5 (length ambient)
  lead <- either (\err -> error ("expected ambient lead lookup to succeed, got " <> err)) pure $
    lookupGeneratedInstrument "ambient" "lead"
  let InstrumentId leadProgram = ipInstrumentId lead
  assertEqual "ambient lead lookup should follow the genre palette" 88 leadProgram
  assertNear "ambient lead amp should come from the generated palette" 0.38 (ipAmp lead)

testTimelineGeneratedInstrumentLookupRejectsUnknownLayer ∷ IO ()
testTimelineGeneratedInstrumentLookupRejectsUnknownLayer =
  case lookupGeneratedInstrument "electronic" "choir" of
    Left err ->
      assertBool "unknown generated layer should explain valid options" ("lead" `isInfixOf` err && "pad" `isInfixOf` err)
    Right inst ->
      error ("expected generated instrument lookup to fail, got " <> show (ipName inst))

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

testTimelineExplainTraceEmitsLookaheadAndBarEvents ∷ IO ()
testTimelineExplainTraceEmitsLookaheadAndBarEvents = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  events <- either (\err -> error ("explain trace failed: " <> err)) pure (drainTimelineExplainEvents testSampleRate 0 spec)
  let lookaheads = [t | TimelineExplainLookahead t <- events]
      bars = [bar | TimelineExplainBar bar <- events]
  assertBool "explain trace should include lookahead events" (not (null lookaheads))
  assertBool "explain trace should include generated bars" (not (null bars))
  case bars of
    bar0 : _ -> do
      assertBool "first explained bar should have notes" (not (null (tbNotes bar0)))
      assertBool "first explained bar should report instrument note counts" (not (null (barInstrumentNoteCounts bar0)))
    [] ->
      error "expected explained bars"

testTimelineExplainTraceEmitsTransitionWeights ∷ IO ()
testTimelineExplainTraceEmitsTransitionWeights = do
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
          , "    pattern_ending: 0/64/1/0.8"
          ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  events <- either (\err -> error ("explain trace failed: " <> err)) pure (drainTimelineExplainEvents testSampleRate 0 spec)
  let transitions = [t | TimelineExplainTransition t <- events]
  case transitions of
    t : _ -> do
      assertEqual "explain transition should start from intro" "intro" (ttFromSectionName t)
      assertEqual "explain transition should use phrase-boundary weighting" "phrase-boundary-weighted" (ttReason t)
      assertBool "explain transition should keep base weights" (not (null (ttBaseWeights t)))
      assertBool "explain transition should keep final weights" (not (null (ttFinalWeights t)))
    [] ->
      error "expected explain trace to emit at least one transition"

testTimelineRapidAdaptiveChangesStayDeterministic ∷ IO ()
testTimelineRapidAdaptiveChangesStayDeterministic = do
  let melodyCsv =
        intercalate "," [show beat <> "/A4/0.5/0.62" | beat <- take 12 ([0.0, 4.0 ..] ∷ [Double])]
      chordsCsv = intercalate "," (take 12 (cycle ["Am", "F", "C", "G"]))
      yamlText =
        unlines
          [ "song:"
          , "  genre: electronic"
          , "  mood: dramatic"
          , "  tempo_bpm: 100"
          , "  beats_per_bar: 4"
          , "sections:"
          , "  ending:"
          , "    bars_per_phrase: 12"
          , "    phrase_count: 1"
          , "    chords: " <> chordsCsv
          , "    melody: " <> melodyCsv
          ]
      changes =
        [ ("ambient", Just "calm", 0.2)
        , ("cinematic", Just "combat", 0.9)
        , ("blackmetal", Just "intense", 0.85)
        , ("electronic", Just "neutral", 0.6)
        , ("ambient", Just "calm", 0.3)
        , ("cinematic", Just "dramatic", 0.95)
        , ("electronic", Just "combat", 0.75)
        , ("blackmetal", Just "combat", 0.9)
        ]
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText yamlText)
  signatures1 <- runAdaptiveSequence changes spec
  signatures2 <- runAdaptiveSequence changes spec
  assertEqual "rapid adaptive changes should stay deterministic" signatures1 signatures2
  assertEqual "rapid adaptive stress run should produce one bar per control step plus the initial bar" (length changes + 1) (length signatures1)
  assertBool "sequence should include electronic lead bars" (any (80 `elem`) signatures1)
  assertBool "sequence should include ambient lead bars" (any (88 `elem`) signatures1)
  assertBool "sequence should include cinematic lead bars" (any (61 `elem`) signatures1)
  assertBool "sequence should include blackmetal lead bars" (any (31 `elem`) signatures1)
  where
    runAdaptiveSequence changes spec = go 0 ((prepareTimelineRuntime testSampleRate 0 spec) { trLookaheadBars = 0 }) changes []

    go now rt pending acc =
      let (batch, rt1) = popReadyBars now rt
      in case batch of
           [bar] ->
             let acc' = acc <> [barPrograms bar]
                 now' = now + tbLengthFrames bar
             in case pending of
                  [] -> pure acc'
                  ((genre, mood, energy) : rest) -> do
                    let rt2 = setTimelineTargets mood energy rt1
                    rt3 <- either (\err -> error ("rapid genre change failed: " <> err)) pure (setTimelineGenre genre rt2)
                    go now' rt3 rest acc'
           [] ->
             error "expected one ready bar during rapid adaptive stress test"
           _ ->
             error "expected zero-lookahead stress test to emit one bar at a time"

    barPrograms bar =
      nub [iid | note <- tbNotes bar, let InstrumentId iid = tnInstrumentId note]

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

testTimelineRetargetTelemetryReportsFutureGenerationPolicy ∷ IO ()
testTimelineRetargetTelemetryReportsFutureGenerationPolicy = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (_batch, runtime1) = popReadyBars 0 runtime0
      runtime2 = setTimelineTargets (Just "combat") 0.9 runtime1
      (events, _runtime3) = popRetargetTelemetry runtime2
      buffered = bufferedFutureBars runtime1
  assertEqual "retarget should emit one policy event" 1 (length events)
  case events of
    [] -> error "expected retarget telemetry event"
    ev : _ -> do
      assertEqual "retarget reason" "live-retarget" (trtReason ev)
      assertEqual "retarget policy" "future-generated-bars-only" (trtPolicy ev)
      assertEqual "buffered bars should be reported" (length buffered) (trtBufferedFutureBars ev)
      assertEqual "next generated bar index should be the current frontier" (trGeneratedBarCount runtime1) (trtNextGeneratedBarIx ev)
      assertEqual "next generated frame should follow the buffered frontier" (trStartFrame runtime1 + trNextBarOffset runtime1) (trtNextGeneratedFrame ev)
      assertEqual "mood target should be captured" (Just "combat") (trtMoodTarget ev)
      assertBool "energy target should be captured" (abs (trtEnergyTarget ev - 0.9) < 0.0001)

testTimelineRetargetDoesNotRewriteBufferedBars ∷ IO ()
testTimelineRetargetDoesNotRewriteBufferedBars = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (_mSpan, runtime1) = timelineBoundarySpanFromNextBar 1 1 runtime0
      bufferedBefore = bufferedFutureBars runtime1
      runtime2 = setTimelineTargets (Just "combat") 0.9 runtime1
      bufferedAfter = bufferedFutureBars runtime2
  assertBool "lookahead should contain buffered bars before retargeting" (not (null bufferedBefore))
  assertEqual "retargeting should preserve already-buffered future bars" bufferedBefore bufferedAfter

testTimelineLookaheadTelemetryReportsBoundaryQueries ∷ IO ()
testTimelineLookaheadTelemetryReportsBoundaryQueries = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  let runtime0 = prepareTimelineRuntime testSampleRate 0 spec
      (_mFrame, runtime1) = timelineBoundarySpanFromNextBar 1 1 runtime0
      (events, _runtime2) = popLookaheadTelemetry runtime1
  assertEqual "boundary query should emit one lookahead event" 1 (length events)
  case events of
    [] -> error "expected lookahead telemetry event"
    ev : _ -> do
      assertEqual "lookahead reason" "next-boundary-span-query" (tltReason ev)
      assertBool "query should generate bars" (tltGeneratedBarCount ev > 0)
      assertEqual "generation should start from bar zero" 0 (tltFirstGeneratedBarIx ev)
      assertEqual "generated count should match inclusive index range" (tltGeneratedBarCount ev - 1) (tltLastGeneratedBarIx ev)
      assertBool "generated frame range should be increasing" (tltEndFrame ev > tltStartFrame ev)
      assertEqual "lookahead should capture the base mood" (Just "calm") (tltMoodTarget ev)
      assertBool "default energy target should be captured" (abs (tltEnergyTarget ev - 0.5) < 0.0001)
      assertEqual "lookahead should capture the active genre" "electronic" (tltGenre ev)
      assertBool "seed should advance during lookahead generation" (tltSeedEnd ev /= tltSeedStart ev)
      assertEqual "pure runtime telemetry should not have measured duration yet" Nothing (tltGenerationDurationUs ev)
      assertEqual "pure runtime telemetry should not have measured throughput yet" Nothing (tltGenerationBarsPerSecond ev)

testTimelineLookaheadPerfMetricsComputeThroughput ∷ IO ()
testTimelineLookaheadPerfMetricsComputeThroughput = do
  let telemetry =
        TimelineLookaheadTelemetry
          { tltReason = "lookahead-horizon"
          , tltGeneratedBarCount = 2
          , tltFirstGeneratedBarIx = 4
          , tltLastGeneratedBarIx = 5
          , tltStartFrame = 120000
          , tltEndFrame = 360000
          , tltBufferedFutureBars = 2
          , tltMoodTarget = Just "calm"
          , tltEnergyTarget = 0.5
          , tltGenre = "electronic"
          , tltSeedStart = 11
          , tltSeedEnd = 29
          , tltGenerationDurationUs = Nothing
          , tltGenerationBarsPerSecond = Nothing
          }
      stamped = stampLookaheadPerfMetrics 2500 telemetry
  assertEqual "perf metrics should capture duration" (Just 2500) (tltGenerationDurationUs stamped)
  case tltGenerationBarsPerSecond stamped of
    Nothing ->
      error "expected bars-per-second metric"
    Just barsPerSecond ->
      assertNear "perf metrics should compute bars per second" 800 barsPerSecond

testTimelineBaselineSummaryAggregatesLookaheadMetrics ∷ IO ()
testTimelineBaselineSummaryAggregatesLookaheadMetrics = do
  let telemetryA =
        (stampLookaheadPerfMetrics 2000
          TimelineLookaheadTelemetry
            { tltReason = "lookahead-horizon"
            , tltGeneratedBarCount = 2
            , tltFirstGeneratedBarIx = 0
            , tltLastGeneratedBarIx = 1
            , tltStartFrame = 0
            , tltEndFrame = 240000
            , tltBufferedFutureBars = 2
            , tltMoodTarget = Just "calm"
            , tltEnergyTarget = 0.5
            , tltGenre = "electronic"
            , tltSeedStart = 1
            , tltSeedEnd = 2
            , tltGenerationDurationUs = Nothing
            , tltGenerationBarsPerSecond = Nothing
            })
      telemetryB =
        (stampLookaheadPerfMetrics 1000
          TimelineLookaheadTelemetry
            { tltReason = "next-boundary-span-query"
            , tltGeneratedBarCount = 1
            , tltFirstGeneratedBarIx = 2
            , tltLastGeneratedBarIx = 2
            , tltStartFrame = 240000
            , tltEndFrame = 360000
            , tltBufferedFutureBars = 1
            , tltMoodTarget = Just "calm"
            , tltEnergyTarget = 0.5
            , tltGenre = "electronic"
            , tltSeedStart = 2
            , tltSeedEnd = 3
            , tltGenerationDurationUs = Nothing
            , tltGenerationBarsPerSecond = Nothing
            })
      summary = summarizeLookaheadTelemetry [telemetryA, telemetryB]
  assertEqual "summary should count lookahead events" 2 (lpsEventCount summary)
  assertEqual "summary should count generated bars" 3 (lpsGeneratedBars summary)
  assertEqual "summary should count measured events" 2 (lpsMeasuredEventCount summary)
  assertEqual "summary should keep peak generation time" 2000 (lpsPeakGenerationUs summary)
  assertEqual "summary should count reasons deterministically" [("lookahead-horizon", 1), ("next-boundary-span-query", 1)] (lpsReasonCounts summary)
  assertEqual "summary should average generation time" (Just 1500) (lookaheadAverageGenerationUs summary)
  case lookaheadAverageBarsPerSecond summary of
    Nothing ->
      error "expected average bars-per-second metric"
    Just avgBarsPerSecond ->
      assertNear "summary should average throughput" 1000 avgBarsPerSecond

testTimelineBaselineScenarioLookupIsCaseInsensitive ∷ IO ()
testTimelineBaselineScenarioLookupIsCaseInsensitive =
  case lookupBaselineScenario "Rapid-Retarget" of
    Nothing ->
      error "expected baseline scenario lookup to succeed"
    Just scenario -> do
      assertEqual "scenario lookup should preserve canonical name" "rapid-retarget" (bsName scenario)
      assertEqual "rapid-retarget should carry its default bar count" 12 (bsDefaultBars scenario)

testTimelineBaselineBarStartsFollowConstantMeter ∷ IO ()
testTimelineBaselineBarStartsFollowConstantMeter = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  assertEqual
    "bar starts should advance by one 120bpm 4/4 bar"
    (Just [0, 96000, 192000])
    (timelineAbsoluteBarStartFrames testSampleRate 0 3 spec)

testTimelineBaselineScenarioSchedulingFollowsBarStarts ∷ IO ()
testTimelineBaselineScenarioSchedulingFollowsBarStarts = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineAdaptivePolicyYaml)
  let scenario =
        BaselineScenario
          { bsName = "test-scenario"
          , bsDescription = "deterministic scheduling test"
          , bsDefaultBars = 3
          , bsSteps =
              [ BaselineScenarioStep 0 (BaselineSetEnergy 0.2)
              , BaselineScenarioStep 2 (BaselineSetMood (Just "combat"))
              , BaselineScenarioStep 4 (BaselineSetGenre (Just "ambient"))
              ]
          }
      scheduled = scheduleBaselineScenario testSampleRate 0 3 spec scenario
  assertEqual
    "scenario scheduler should keep only in-range steps and map them to bar starts"
    [ (0, 0, BaselineSetEnergy 0.2)
    , (2, 192000, BaselineSetMood (Just "combat"))
    ]
    [ (sbaBarIndex action, sbaFrame action, sbaAction action)
    | action <- scheduled
    ]

testTimelineBaselineEndFrameFollowsRequestedBars ∷ IO ()
testTimelineBaselineEndFrameFollowsRequestedBars = do
  spec <- either (\err -> error ("parse failed: " <> err)) pure (parseSongSpecText timelineTempoChangeYaml)
  assertEqual
    "baseline helper should stop after the requested number of bars"
    (Just 96000)
    (timelineAbsoluteEndFrameForBars testSampleRate 0 1 spec)

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

timelineAdaptivePolicyYaml ∷ String
timelineAdaptivePolicyYaml =
  unlines
    [ "song:"
    , "  genre: electronic"
    , "  mood: calm"
    , "  lookahead_bars: 2"
    , "sections:"
    , "  ending:"
    , "    tempo_bpm: 120"
    , "    beats_per_bar: 4"
    , "    bars_per_phrase: 6"
    , "    phrase_count: 1"
    , "instruments:"
    , "  pulse:"
    , "    instrument_id: 0"
    , "    amp: 1"
    , "    pan: 0"
    , "    pattern_ending: 0/60/0.5/0.9,0.5/62/0.5/0.9,1/64/0.5/0.9,1.5/65/0.5/0.9,2/67/0.5/0.9,2.5/69/0.5/0.9,3/71/0.5/0.9,3.5/72/0.5/0.9"
    ]

bufferedFutureBars ∷ TimelineRuntime → [TimelineBar]
bufferedFutureBars rt =
  let seqIx = max 0 (trNextBarIx rt - trBarOffset rt)
  in drop seqIx (toList (trBars rt))

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
