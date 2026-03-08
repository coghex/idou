{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import Data.List (find, intercalate, isInfixOf)
import Data.Word (Word32)
import Foreign.ForeignPtr (mallocForeignPtrArray)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.Vector.Mutable as MV

import Audio.Config (AudioBufferConfig(..), AudioConfig(..), parseAudioConfigText)
import Audio.Envelope
import Audio.Thread.InstrumentTable
  ( MidiControls(..)
  , lookupMidiControls
  , resetMidiControls
  , setChannelPan
  , setChannelVolume
  , setExpression
  , setInstrument
  , setModWheel
  , setPitchBend
  )
import Audio.Thread.Types (AudioState(..), Voice(..))
import Audio.Thread.Voice (addInstrumentNote, releaseInstrumentAllVoices, releaseInstrumentNote)
import Audio.Types
import Midi.Control (controllerPanValue, controllerValue01, midiPitchBendSemitones, sustainPedalDown)
import Engine.Core.Queue (newQueue)

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
  , TestCase "release-targets-matching-note-instance" testReleaseTargetsMatchingNoteInstance
  , TestCase "poly-voice-steal-reuses-released-slot" testPolyVoiceStealReusesReleasedSlot
  , TestCase "release-all-voices-keeps-other-instruments-active" testReleaseAllVoicesKeepsOtherInstrumentsActive
  , TestCase "midi-controller-state-clamps-and-resets" testMidiControllerStateClampsAndResets
  , TestCase "midi-controller-conversions-are-normalized" testMidiControllerConversionsAreNormalized
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
  st5 <- setPitchBend iid (defaultPitchBendRangeSemitones * 2) st4
  ctrls <- lookupMidiControls iid st5
  assertEqual
    "clamped controller state"
    MidiControls
      { mcChannelVolume = 1
      , mcExpression = 0
      , mcChannelPan = 1
      , mcModWheel = 0.4
      , mcPitchBendSemis = defaultPitchBendRangeSemitones
      }
    ctrls
  st6 <- resetMidiControls iid st5
  resetCtrls <- lookupMidiControls iid st6
  assertEqual
    "reset controller state"
    MidiControls
      { mcChannelVolume = 1
      , mcExpression = 1
      , mcChannelPan = 0
      , mcModWheel = 0
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

mkTestHandle ∷ IO AudioHandle
mkTestHandle = do
  q <- newQueue
  pure AudioHandle
    { audioQueue = q
    , sampleRate = testSampleRate
    , channels = 2
    }

mkTestState ∷ IO AudioState
mkTestState = do
  voices <- MV.new 8
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
  pitchBendVec <- MV.replicate 256 0
  pitchModScratch <- MV.replicate 4 1
  pitchCentsScratch <- MV.replicate 4 0
  pure AudioState
    { stVoices = voices
    , stActiveCount = 0
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
    , stPitchBendSemis = pitchBendVec
    , stNow = 0
    }

mkInstrument ∷ PlayMode → Instrument
mkInstrument playMode =
  Instrument
    { iOscs = [OscLayer WaveSine (PitchSpec 0 0 0 0) 1 NoSync]
    , iLayerSpread = 0
    , iAdsrDefault = ADSR 0.01 0.02 0.5 0.03
    , iGain = 1
    , iFilter = Nothing
    , iModRoutes = []
    , iPlayMode = playMode
    , iPolyMax = 8
    , iVoiceSteal = StealQuietest
    }

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
