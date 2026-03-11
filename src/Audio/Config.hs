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

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Word (Word32)

import qualified Data.Map.Strict as M

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

type FlatYaml = Map [String] String

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

parseFlatYaml ∷ String → Either String FlatYaml
parseFlatYaml contents = snd <$> foldl step (Right ([], M.empty)) (zip [1 :: Int ..] (lines contents))
  where
    step
      ∷ Either String ([String], FlatYaml)
      → (Int, String)
      → Either String ([String], FlatYaml)
    step acc (lineNo, rawLine) = do
      (contexts, entries) <- acc
      let line = stripComment rawLine
          trimmed = dropWhile isSpace line
      if null trimmed
        then pure (contexts, entries)
        else do
          let indent = length line - length trimmed
          ensure (indent `mod` 2 == 0) ("line " <> show lineNo <> ": indentation must use multiples of two spaces")
          let level = indent `div` 2
          ensure (level <= length contexts) ("line " <> show lineNo <> ": invalid nesting level")
          (key, mValue) <- parseKeyValue lineNo trimmed
          let parent = take level contexts
              path = parent <> [key]
          case mValue of
            Nothing ->
              pure (path, entries)
            Just value -> do
              ensure (not (M.member path entries)) ("line " <> show lineNo <> ": duplicate key " <> renderPath path)
              pure (parent, M.insert path value entries)

parseKeyValue ∷ Int → String → Either String (String, Maybe String)
parseKeyValue lineNo content =
  case break (== ':') content of
    (_, []) ->
      Left ("line " <> show lineNo <> ": expected key: value")
    (rawKey, ':' : rawValue) -> do
      let key = trim rawKey
          value = trim rawValue
      ensure (not (null key)) ("line " <> show lineNo <> ": empty key")
      pure (key, if null value then Nothing else Just value)
    _ ->
      Left ("line " <> show lineNo <> ": expected key: value")

stripComment ∷ String → String
stripComment = reverse . dropWhile isSpace . reverse . takeWhile (/= '#')

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse

rejectUnexpectedKeys ∷ FlatYaml → Either String ()
rejectUnexpectedKeys entries = do
  let keys = M.keys entries
      unexpected = filter (`notElem` allowedLeafKeys) keys
      missing = filter (`M.notMember` entries) requiredLeafKeys
  ensure (null unexpected) ("unexpected config keys: " <> intercalate ", " (map renderPath unexpected))
  ensure (null missing) ("missing config keys: " <> intercalate ", " (map renderPath missing))

lookupIntKey ∷ [String] → FlatYaml → Either String Int
lookupIntKey key entries = do
  raw <- lookupKey key entries
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid integer for " <> renderPath key <> ": " <> raw)

lookupWord32Key ∷ [String] → FlatYaml → Either String Word32
lookupWord32Key key entries = do
  n <- lookupIntKey key entries
  ensure (n >= 0) ("value for " <> renderPath key <> " must be non-negative")
  ensure (n <= fromIntegral (maxBound ∷ Word32))
    ("value for " <> renderPath key <> " exceeds maximum (" <> show (maxBound ∷ Word32) <> ")")
  pure (fromIntegral n)

lookupIntKeyDefault ∷ [String] → Int → FlatYaml → Either String Int
lookupIntKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupIntKey key entries

lookupKey ∷ [String] → FlatYaml → Either String String
lookupKey key entries =
  case M.lookup key entries of
    Just value -> pure value
    Nothing -> Left ("missing config key " <> renderPath key)

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

ensure ∷ Bool → String → Either String ()
ensure cond err =
  if cond
    then pure ()
    else Left err

renderPath ∷ [String] → String
renderPath = intercalate "."
