{-# LANGUAGE Strict, UnicodeSyntax #-}

module Tests.ConfigPatch (testCases) where

import Data.List (find, isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)

import Audio.Config
  ( AudioBufferConfig(..)
  , AudioConfig(..)
  , AudioTelemetryConfig(..)
  , loadAudioConfigEither
  , parseAudioConfigText
  )
import Audio.Envelope (aAttackSec, aDecaySec, aReleaseSec, aSustain)
import Audio.Filter.Biquad (FilterType(..))
import Audio.Filter.Types (FilterSpec(..), FilterSlope(..), FilterTarget(..), KeyTrack(..))
import Audio.Patch.Loader (compilePatchText, loadInstrumentPatch, loadInstrumentPatchEither)
import Audio.Types
import TestSupport (TestCase(..), assertBool, assertEqual, assertLeftContains, assertNear, catchIgnore)

testCases ∷ [TestCase]
testCases =
  [ TestCase "audio-config-parses-yaml-shape" testAudioConfigParsesYamlShape
  , TestCase "audio-config-rejects-too-small-buffer" testAudioConfigRejectsTooSmallBuffer
  , TestCase "audio-config-load-reports-missing-file" testAudioConfigLoadReportsMissingFile
  , TestCase "patch-loader-compiles-voice-local-graph" testPatchLoaderCompilesVoiceLocalGraph
  , TestCase "patch-loader-compiles-inline-layer-envelope" testPatchLoaderCompilesInlineLayerEnvelope
  , TestCase "patch-loader-rejects-inline-envelope-connection-conflicts" testPatchLoaderRejectsInlineEnvelopeConnectionConflicts
  , TestCase "patch-loader-rejects-invalid-filter-layer-target" testPatchLoaderRejectsInvalidFilterLayerTarget
  , TestCase "patch-loader-rejects-inactive-modulation-targets" testPatchLoaderRejectsInactiveModulationTargets
  , TestCase "patch-loader-rejects-too-many-modulation-routes" testPatchLoaderRejectsTooManyModulationRoutes
  , TestCase "patch-loader-rejects-too-many-layers" testPatchLoaderRejectsTooManyLayers
  , TestCase "patch-loader-rejects-mixed-dry-and-filtered-routing" testPatchLoaderRejectsMixedDryAndFilteredRouting
  , TestCase "patch-loader-load-reports-missing-file" testPatchLoaderLoadReportsMissingFile
  , TestCase "patch-loader-loads-sample-file" testPatchLoaderLoadsSampleFile
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

testAudioConfigLoadReportsMissingFile ∷ IO ()
testAudioConfigLoadReportsMissingFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir <> "/idou-missing-audio-config.yaml"
  catchIgnore (removeFile path)
  loaded <- loadAudioConfigEither path
  case loaded of
    Left err ->
      assertBool "expected audio config read error" ("Failed to read audio config" `isInfixOf` err)
    Right cfg ->
      error ("expected missing audio config failure, got " <> show cfg)

testPatchLoaderCompilesVoiceLocalGraph ∷ IO ()
testPatchLoaderCompilesVoiceLocalGraph = do
  let patchText =
        unlines
          [ "patch:"
          , "  name: Motion Pad"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "      level: 0.62"
          , "      pitch:"
          , "        cents: -7"
          , "    osc2:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "      level: 0.58"
          , "      pitch:"
          , "        cents: 7"
          , "    noise1:"
          , "      type: noise"
          , "      color: mix"
          , "      mix: 0.35"
          , "      level: 0.08"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.08"
          , "      decay: 1.20"
          , "      sustain: 0.72"
          , "      release: 1.80"
          , "    noise_env:"
          , "      type: envelope"
          , "      attack: 0.00"
          , "      decay: 0.04"
          , "      sustain: 0.00"
          , "      release: 0.05"
          , "    filter_env:"
          , "      type: envelope"
          , "      attack: 0.02"
          , "      decay: 0.90"
          , "      sustain: 0.14"
          , "      release: 1.30"
          , "    filter1:"
          , "      type: filter"
          , "      mode: lowpass"
          , "      cutoff_hz: 1800"
          , "      q: 0.74"
           , "      slope: 24"
           , "      key_track: 0.38"
           , "      layer_target: osc1"
           , "      env_amount_oct: 1.45"
           , "      q_env_amount: 0.08"
           , "    out:"
           , "      type: output"
           , "      gain: 0.82"
           , "      layer_spread: 0.18"
           , "      lfo1_rate_hz: 4.5"
           , "      play_mode: poly"
           , "      poly_max: 12"
           , "  modulations:"
           , "    amp_from_key_track:"
           , "      source: key_track"
           , "      target: out"
           , "      destination: gain"
           , "      amount: -0.08"
           , "    amp_from_poly_pressure:"
           , "      source: poly_aftertouch"
           , "      target: out"
           , "      destination: amp_gain"
           , "      amount: 0.10"
           , "    filter_from_filter_env:"
           , "      source: filter_envelope"
           , "      target: filter1"
           , "      destination: cutoff_oct"
           , "      amount: 0.35"
           , "    filter_from_pressure:"
           , "      source: channel_aftertouch"
           , "      target: filter1"
           , "      destination: filter_cutoff_oct"
           , "      amount: 0.22"
           , "    pitch_from_amp_env:"
           , "      source: amp_envelope"
           , "      target: osc1"
           , "      destination: layer_pitch_cents"
           , "      amount: -6"
           , "    pitch_from_lfo:"
           , "      source: lfo1"
           , "      target: osc2"
           , "      destination: pitch_cents"
           , "      amount: 14"
           , "  connections:"
           , "    osc1_to_filter:"
           , "      from: osc1"
          , "      to: filter1"
          , "      kind: signal"
          , "    osc2_to_filter:"
          , "      from: osc2"
          , "      to: filter1"
          , "      kind: signal"
          , "    noise_to_filter:"
          , "      from: noise1"
          , "      to: filter1"
          , "      kind: signal"
          , "    filter_to_out:"
          , "      from: filter1"
          , "      to: out"
          , "      kind: signal"
          , "    amp_to_out:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          , "    noise_env_to_noise:"
          , "      from: noise_env"
          , "      to: noise1"
          , "      kind: amp_envelope"
          , "    filter_env_to_filter:"
          , "      from: filter_env"
          , "      to: filter1"
          , "      kind: filter_envelope"
          , "    osc1_syncs_osc2:"
          , "      from: osc1"
          , "      to: osc2"
          , "      kind: hard_sync"
          ]
  case compilePatchText patchText of
    Left err ->
      error ("expected patch compile success, got " <> err)
    Right instrument -> do
      assertEqual "compiled patch should create three layers" 3 (length (iOscs instrument))
      assertNear "output gain" 0.82 (iGain instrument)
      assertNear "layer spread" 0.18 (iLayerSpread instrument)
      assertNear "patch should compile output lfo1 rate" 4.5 (iLfo1RateHz instrument)
      assertEqual "patch should stay polyphonic" Poly (iPlayMode instrument)
      assertEqual "patch should preserve poly max" 12 (iPolyMax instrument)
      assertEqual "patch compiler should compile six modulation routes" 6 (length (iModRoutes instrument))
      assertEqual "output ADSR attack" 0.08 (aAttackSec (iAdsrDefault instrument))
      assertBool
        "patch should compile lfo pitch modulation for osc2"
        (find (== ModRoute ModSrcLfo1 (ModDstLayerPitchCents 2) 14) (iModRoutes instrument) /= Nothing)
      assertBool
        "patch should compile amp-envelope pitch modulation for osc1"
        (find (== ModRoute ModSrcEnvAmp (ModDstLayerPitchCents 1) (-6)) (iModRoutes instrument) /= Nothing)
      assertBool
        "patch should compile filter-envelope cutoff modulation"
        (find (== ModRoute ModSrcEnvFilter ModDstFilterCutoffOct 0.35) (iModRoutes instrument) /= Nothing)
      assertBool
        "patch should compile channel-aftertouch cutoff modulation"
        (find (== ModRoute ModSrcChanAftertouch ModDstFilterCutoffOct 0.22) (iModRoutes instrument) /= Nothing)
      assertBool
        "patch should compile key-track amp modulation"
        (find (== ModRoute ModSrcKeyTrack ModDstAmpGain (-0.08)) (iModRoutes instrument) /= Nothing)
      assertBool
        "patch should compile poly-aftertouch amp modulation"
        (find (== ModRoute ModSrcPolyAftertouch ModDstAmpGain 0.10) (iModRoutes instrument) /= Nothing)
      case iFilter instrument of
        Nothing ->
          error "expected compiled patch to include a filter"
        Just filterSpec -> do
          assertEqual "filter mode" FLP (fType filterSpec)
          assertEqual "filter slope" S24 (fSlope filterSpec)
          assertEqual "filter key track" (KeyTrack 0.38) (fKeyTrack filterSpec)
          assertEqual "filter target should compile to osc1's active layer" (FilterTargetLayer 1) (fTarget filterSpec)
          assertNear "filter env amount" 1.45 (fEnvAmountOct filterSpec)
          assertNear "filter q env amount" 0.08 (fQEnvAmount filterSpec)
          assertEqual "filter env release" 1.30 (aReleaseSec (fEnvADSR filterSpec))
      let syncedLayer =
            find
              (\layer ->
                psCents (olPitch layer) > 6 &&
                case olSync layer of
                  HardSyncTo _ -> True
                  _ -> False
              )
              (iOscs instrument)
      case syncedLayer of
        Nothing ->
          error "expected one oscillator layer to use hard sync"
        Just layer ->
          assertNear "synced oscillator cents" 7 (psCents (olPitch layer))
      let noiseLayer =
            find
              (\layer ->
                case olWaveform layer of
                  WaveNoiseMix _ -> True
                  _ -> False
              )
              (iOscs instrument)
      case noiseLayer of
        Nothing ->
          error "expected one noise layer"
        Just layer -> do
          assertNear "noise layer level" 0.08 (olLevel layer)
          case olAmpEnv layer of
            Nothing ->
              error "expected noise layer to use its own amp envelope"
            Just adsr ->
              assertNear "noise env decay" 0.04 (aDecaySec adsr)

testPatchLoaderCompilesInlineLayerEnvelope ∷ IO ()
testPatchLoaderCompilesInlineLayerEnvelope = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "      amp_envelope:"
          , "        attack: 0.03"
          , "        decay: 0.12"
          , "        sustain: 0.45"
          , "        release: 0.70"
          , "    noise1:"
          , "      type: noise"
          , "      color: mix"
          , "      mix: 0.4"
          , "      amp_envelope:"
          , "        attack: 0.00"
          , "        decay: 0.08"
          , "        sustain: 0.00"
          , "        release: 0.06"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    out:"
          , "      type: output"
          , "  connections:"
          , "    osc:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    noise:"
          , "      from: noise1"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  case compilePatchText patchText of
    Left err ->
      error ("expected inline layer envelope patch to compile, got " <> err)
    Right instrument -> do
      let oscLayer = find (\layer -> not (isNoiseWave layer) && olAmpEnv layer /= Nothing) (iOscs instrument)
          noiseLayer = find (\layer -> isNoiseWave layer && olAmpEnv layer /= Nothing) (iOscs instrument)
      case oscLayer of
        Nothing -> error "expected oscillator inline amp envelope"
        Just layer ->
          case olAmpEnv layer of
            Nothing -> error "expected oscillator inline amp envelope"
            Just adsr -> do
              assertNear "inline oscillator env attack" 0.03 (aAttackSec adsr)
              assertNear "inline oscillator env sustain" 0.45 (aSustain adsr)
      case noiseLayer of
        Nothing -> error "expected noise inline amp envelope"
        Just layer ->
          case olAmpEnv layer of
            Nothing -> error "expected noise inline amp envelope"
            Just adsr -> do
              assertNear "inline noise env decay" 0.08 (aDecaySec adsr)
              assertNear "inline noise env release" 0.06 (aReleaseSec adsr)

testPatchLoaderRejectsInlineEnvelopeConnectionConflicts ∷ IO ()
testPatchLoaderRejectsInlineEnvelopeConnectionConflicts = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "      amp_envelope:"
          , "        attack: 0.02"
          , "        decay: 0.15"
          , "        sustain: 0.50"
          , "        release: 0.60"
          , "    osc_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    out:"
          , "      type: output"
          , "  connections:"
          , "    osc:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    osc_env_to_osc:"
          , "      from: osc_env"
          , "      to: osc1"
          , "      kind: amp_envelope"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains
    "inline layer envelopes should not conflict with explicit envelope connections"
    "cannot use both an inline amp_envelope block and an amp_envelope connection"
    (compilePatchText patchText)

testPatchLoaderRejectsInactiveModulationTargets ∷ IO ()
testPatchLoaderRejectsInactiveModulationTargets = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    filter1:"
          , "      type: filter"
          , "      mode: lowpass"
          , "      cutoff_hz: 1200"
          , "    out:"
          , "      type: output"
          , "  modulations:"
          , "    bad_filter_route:"
          , "      source: channel_aftertouch"
          , "      target: filter1"
          , "      destination: filter_cutoff_oct"
          , "      amount: 0.25"
          , "  connections:"
          , "    c1:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains
    "filter modulation should target the active filter path"
    "patch has no active filter"
    (compilePatchText patchText)

testPatchLoaderRejectsInvalidFilterLayerTarget ∷ IO ()
testPatchLoaderRejectsInvalidFilterLayerTarget = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    filter1:"
          , "      type: filter"
          , "      mode: lowpass"
          , "      cutoff_hz: 1200"
          , "      layer_target: amp_env"
          , "    out:"
          , "      type: output"
          , "  connections:"
          , "    wet_in:"
          , "      from: osc1"
          , "      to: filter1"
          , "      kind: signal"
          , "    wet_out:"
          , "      from: filter1"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains
    "filter layer target should require an active oscillator or noise node"
    "layer_target must reference an active oscillator or noise node"
    (compilePatchText patchText)

testPatchLoaderRejectsTooManyModulationRoutes ∷ IO ()
testPatchLoaderRejectsTooManyModulationRoutes = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    out:"
          , "      type: output"
          , "  modulations:"
          , "    r1:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.01"
          , "    r2:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.02"
          , "    r3:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.03"
          , "    r4:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.04"
          , "    r5:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.05"
          , "    r6:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.06"
          , "    r7:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.07"
          , "    r8:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.08"
          , "    r9:"
          , "      source: key_track"
          , "      target: out"
          , "      destination: amp_gain"
          , "      amount: 0.09"
          , "  connections:"
          , "    c1:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains
    "patch loader should reject more modulation routes than the renderer evaluates"
    "supports at most 8"
    (compilePatchText patchText)

testPatchLoaderRejectsTooManyLayers ∷ IO ()
testPatchLoaderRejectsTooManyLayers = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    osc2:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    osc3:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    osc4:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    osc5:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    out:"
          , "      type: output"
          , "  connections:"
          , "    c1:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    c2:"
          , "      from: osc2"
          , "      to: out"
          , "      kind: signal"
          , "    c3:"
          , "      from: osc3"
          , "      to: out"
          , "      kind: signal"
          , "    c4:"
          , "      from: osc4"
          , "      to: out"
          , "      kind: signal"
          , "    c5:"
          , "      from: osc5"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains "too many patch layers should be rejected" "at most 4" (compilePatchText patchText)

testPatchLoaderRejectsMixedDryAndFilteredRouting ∷ IO ()
testPatchLoaderRejectsMixedDryAndFilteredRouting = do
  let patchText =
        unlines
          [ "patch:"
          , "  nodes:"
          , "    osc1:"
          , "      type: oscillator"
          , "      waveform: saw"
          , "    osc2:"
          , "      type: oscillator"
          , "      waveform: square"
          , "    amp_env:"
          , "      type: envelope"
          , "      attack: 0.01"
          , "      decay: 0.10"
          , "      sustain: 0.80"
          , "      release: 0.30"
          , "    filter1:"
          , "      type: filter"
          , "      mode: lowpass"
          , "      cutoff_hz: 1200"
          , "    out:"
          , "      type: output"
          , "  connections:"
          , "    dry:"
          , "      from: osc1"
          , "      to: out"
          , "      kind: signal"
          , "    wet_in:"
          , "      from: osc2"
          , "      to: filter1"
          , "      kind: signal"
          , "    wet_out:"
          , "      from: filter1"
          , "      to: out"
          , "      kind: signal"
          , "    env:"
          , "      from: amp_env"
          , "      to: out"
          , "      kind: amp_envelope"
          ]
  assertLeftContains
    "mixed dry and filtered routing should be rejected"
    "cannot mix direct output and filtered output"
    (compilePatchText patchText)

testPatchLoaderLoadsSampleFile ∷ IO ()
testPatchLoaderLoadsSampleFile = do
  instrument <- loadInstrumentPatch "config/patches/warm-pad.yaml"
  assertEqual "sample patch should compile into three layers" 3 (length (iOscs instrument))
  assertBool "sample patch should include a filter" (iFilter instrument /= Nothing)
  assertNear "sample patch should preserve output lfo1 rate" 4.5 (iLfo1RateHz instrument)
  case iFilter instrument of
    Nothing -> error "expected sample patch filter"
    Just filterSpec ->
      assertEqual "sample patch should preserve its layer-targeted filter" (FilterTargetLayer 1) (fTarget filterSpec)
  assertBool "sample patch should expose channel aftertouch modulation" (hasChannelAftertouchRoute (iModRoutes instrument))
  assertBool "sample patch should preserve its inline noise envelope" (any (\layer -> isNoiseWave layer && olAmpEnv layer /= Nothing) (iOscs instrument))

hasChannelAftertouchRoute ∷ [ModRoute] → Bool
hasChannelAftertouchRoute = any ((== ModSrcChanAftertouch) . mrSrc)

isNoiseWave ∷ OscLayer → Bool
isNoiseWave layer =
  case olWaveform layer of
    WaveWhiteNoise -> True
    WavePinkNoise -> True
    WaveNoiseMix _ -> True
    _ -> False

testPatchLoaderLoadReportsMissingFile ∷ IO ()
testPatchLoaderLoadReportsMissingFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir <> "/idou-missing-patch.yaml"
  catchIgnore (removeFile path)
  loaded <- loadInstrumentPatchEither path
  case loaded of
    Left err ->
      assertBool "expected patch read error" ("Failed to read patch" `isInfixOf` err)
    Right instrument ->
      error ("expected missing patch failure, got " <> show instrument)
