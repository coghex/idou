{-# LANGUAGE Strict, UnicodeSyntax #-}

module TestSupport
  ( TestCase(..)
  , assertBool
  , assertEqual
  , assertLeftContains
  , assertNear
  , catchIgnore
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
  , mkWav16Mono
  , noiseLayerHasEnvelope
  , noteVelocityAtFrame
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
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.List (find, isInfixOf)
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (mallocForeignPtrArray)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openBinaryTempFile, hClose)

import qualified Data.Vector.Mutable as MV

import Audio.Envelope
import Audio.Oscillator (Osc(..), oscStep)
import Audio.Thread.Types (AudioState(..), ClipSource(..), Voice(..))
import Audio.Thread.Voice
  ( addInstrumentNote
  , panGains
  , releaseInstrumentNote
  , stopVoicesByBus
  )
import Audio.Types
import Engine.Core.Queue (Queue, newQueue, tryReadQueue)
import Player.Timeline (TimelineNote(..))
import Sound.Miniaudio.RingBuffer (MaRB, rbReadF32)

data TestCase = TestCase
  { tcName   ∷ String
  , tcAction ∷ IO ()
  }

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
    ScheduledNoteOn iid bus amp pan key noteInst vel adsrOverride -> do
      handle <- mkTestHandle
      addInstrumentNote handle st iid bus amp pan key noteInst vel adsrOverride
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
      stopVoicesByBus bus st >>= go 0
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
mkTestState = mkTestStateWithVoices 8

mkTestStateWithVoices ∷ Int → IO AudioState
mkTestStateWithVoices voiceCapacity = do
  let cap = max 1 voiceCapacity
  voices <- MV.new cap
  clipAssetVec <- MV.replicate 1024 Nothing
  clipSourceVec <- MV.new cap
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
    , iLfo1RateHz = 0
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

readRenderedSamples ∷ Ptr MaRB → Int → IO [Float]
readRenderedSamples rb frames =
  allocaArray (frames * 2) $ \buf -> do
    got <- rbReadF32 rb buf (fromIntegral frames)
    assertEqual "ring buffer should contain requested frames" frames (fromIntegral got ∷ Int)
    map realToFrac <$> peekArray (frames * 2) buf

takeEveryOther ∷ [a] → [a]
takeEveryOther [] = []
takeEveryOther (x:_:rest) = x : takeEveryOther rest
takeEveryOther [x] = [x]

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

assertLeftContains ∷ Show a => String → String → Either String a → IO ()
assertLeftContains label needle result =
  case result of
    Left err ->
      unless (needle `isInfixOf` err) $
        error (label <> ": expected error containing " <> show needle <> ", got " <> show err)
    Right value ->
      error (label <> ": expected failure, got " <> show value)

assertNear ∷ String → Float → Float → IO ()
assertNear label expected actual =
  unless (abs (expected - actual) < 1e-4) $
    error (label <> ": expected " <> show expected <> ", got " <> show actual)
