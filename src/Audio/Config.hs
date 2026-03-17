{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Config
  ( AudioConfig(..)
  , AudioBufferConfig(..)
  , AudioTelemetryConfig(..)
  , audioConfigPath
  , defaultAudioConfig
  , loadAudioConfig
  , parseAudioConfigText
  ) where
import Data.List (intercalate)
import Data.Word (Word32)

import qualified Data.Map.Strict as M

import Engine.Config.FlatYaml
  ( FlatYaml
  , ensure
  , lookupIntKey
  , lookupIntKeyDefault
  , lookupWord32Key
  , parseFlatYaml
  , renderPath
  )

data AudioBufferConfig = AudioBufferConfig
  { abCapacityFrames ∷ !Word32
  , abTargetChunks   ∷ !Int
  } deriving (Eq, Show)

data AudioConfig = AudioConfig
  { acSampleRate  ∷ !Word32
  , acChunkFrames ∷ !Int
  , acMaxVoices   ∷ !Int
  , acBuffer      ∷ !AudioBufferConfig
  , acTelemetry   ∷ !AudioTelemetryConfig
  } deriving (Eq, Show)

data AudioTelemetryConfig = AudioTelemetryConfig
  { atVerboseReportEveryLoops   ∷ !Int
  , atPartialWriteAlertThreshold ∷ !Int
  } deriving (Eq, Show)

audioConfigPath ∷ FilePath
audioConfigPath = "config/audio.yaml"

defaultAudioConfig ∷ AudioConfig
defaultAudioConfig =
  AudioConfig
    { acSampleRate = 48000
    , acChunkFrames = 256
    , acMaxVoices = 256
    , acBuffer =
        AudioBufferConfig
          { abCapacityFrames = 8192
          , abTargetChunks = 8
          }
    , acTelemetry =
        AudioTelemetryConfig
          { atVerboseReportEveryLoops = 1000
          , atPartialWriteAlertThreshold = 1
          }
    }

loadAudioConfig ∷ FilePath → IO AudioConfig
loadAudioConfig path = do
  contents <- readFile path
  case parseAudioConfigText contents of
    Left err ->
      ioError (userError ("Invalid audio config in " <> path <> ": " <> err))
    Right cfg ->
      pure cfg

parseAudioConfigText ∷ String → Either String AudioConfig
parseAudioConfigText contents = do
  entries <- parseFlatYaml contents
  rejectUnexpectedKeys entries
  sampleRate <- lookupWord32Key ["audio", "sample_rate"] entries
  chunkFrames <- lookupIntKey ["audio", "chunk_frames"] entries
  maxVoices <- lookupIntKey ["audio", "max_voices"] entries
  capacityFrames <- lookupWord32Key ["audio", "buffer", "capacity_frames"] entries
  targetChunks <- lookupIntKey ["audio", "buffer", "target_chunks"] entries
  verboseEvery <-
    lookupIntKeyDefault
      ["audio", "telemetry", "verbose_report_every_loops"]
      (atVerboseReportEveryLoops (acTelemetry defaultAudioConfig))
      entries
  partialWriteThreshold <-
    lookupIntKeyDefault
      ["audio", "telemetry", "partial_write_alert_threshold"]
      (atPartialWriteAlertThreshold (acTelemetry defaultAudioConfig))
      entries
  let cfg =
        AudioConfig
          { acSampleRate = sampleRate
          , acChunkFrames = chunkFrames
          , acMaxVoices = maxVoices
          , acBuffer =
              AudioBufferConfig
                { abCapacityFrames = capacityFrames
                , abTargetChunks = targetChunks
                }
          , acTelemetry =
              AudioTelemetryConfig
                { atVerboseReportEveryLoops = verboseEvery
                , atPartialWriteAlertThreshold = partialWriteThreshold
                }
          }
  validateAudioConfig cfg

requiredLeafKeys ∷ [[String]]
requiredLeafKeys =
  [ ["audio", "sample_rate"]
  , ["audio", "chunk_frames"]
  , ["audio", "max_voices"]
  , ["audio", "buffer", "capacity_frames"]
  , ["audio", "buffer", "target_chunks"]
  ]

optionalLeafKeys ∷ [[String]]
optionalLeafKeys =
  [ ["audio", "telemetry", "verbose_report_every_loops"]
  , ["audio", "telemetry", "partial_write_alert_threshold"]
  ]

allowedLeafKeys ∷ [[String]]
allowedLeafKeys = requiredLeafKeys <> optionalLeafKeys

rejectUnexpectedKeys ∷ FlatYaml → Either String ()
rejectUnexpectedKeys entries = do
  let keys = M.keys entries
      unexpected = filter (`notElem` allowedLeafKeys) keys
      missing = filter (`M.notMember` entries) requiredLeafKeys
  ensure (null unexpected) ("unexpected config keys: " <> intercalate ", " (map renderPath unexpected))
  ensure (null missing) ("missing config keys: " <> intercalate ", " (map renderPath missing))

validateAudioConfig ∷ AudioConfig → Either String AudioConfig
validateAudioConfig cfg = do
  ensure (acSampleRate cfg > 0) "audio.sample_rate must be greater than 0"
  ensure (acChunkFrames cfg > 0) "audio.chunk_frames must be greater than 0"
  ensure (acMaxVoices cfg > 0) "audio.max_voices must be greater than 0"
  let bufferCfg = acBuffer cfg
      targetFrames = toInteger (acChunkFrames cfg) * toInteger (abTargetChunks bufferCfg)
  ensure (abCapacityFrames bufferCfg > 0) "audio.buffer.capacity_frames must be greater than 0"
  ensure (abTargetChunks bufferCfg > 0) "audio.buffer.target_chunks must be greater than 0"
  ensure
    (targetFrames <= toInteger (abCapacityFrames bufferCfg))
    "audio.buffer.capacity_frames must be at least chunk_frames * target_chunks"
  let telemetryCfg = acTelemetry cfg
  ensure (atVerboseReportEveryLoops telemetryCfg > 0) "audio.telemetry.verbose_report_every_loops must be greater than 0"
  ensure (atPartialWriteAlertThreshold telemetryCfg > 0) "audio.telemetry.partial_write_alert_threshold must be greater than 0"
  pure cfg
