{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline.Parse
  ( loadSongSpec
  , parseSongSpecText
  , validateSongGenre
  , generatedInstrumentSpecs
  , lookupGeneratedInstrument
  , arrangeGeneratedInstruments
  , instrumentPatchOverrides
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (find, isInfixOf, nub, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), isAbsolute, normalise, takeDirectory)

import qualified Data.Map.Strict as M

import Audio.Types (InstrumentId(..), NoteKey(..))

import Player.Timeline.Types

loadSongSpec ∷ FilePath → IO (Either String SongSpec)
loadSongSpec path = do
  textResult <- try (readFile path >>= \text -> evaluate (length text) >> pure text) ∷ IO (Either SomeException String)
  pure $
    case textResult of
      Left ex ->
        Left ("Failed to read song spec " <> path <> ": " <> displayException ex)
      Right text ->
        parseSongSpecText text >>= resolveSongSpecPatchPaths (takeDirectory path)

parseSongSpecText ∷ String → Either String SongSpec
parseSongSpecText contents = do
  entries <- parseFlatYaml contents
  let modeRaw = map toLower (lookupTextDefault ["song", "mode"] "cue" entries)
      genre = normalizeName (lookupTextDefault ["song", "genre"] "" entries)
      mood = normalizeName (lookupTextDefault ["song", "mood"] "" entries)
      form = parseNameList (lookupTextDefault ["song", "form"] "" entries)
      patchOverrides = parseGeneratedPatchOverrides entries
  lookaheadBars <- max 1 <$> lookupIntKeyDefault ["song", "lookahead_bars"] 2 entries
  mode <-
    case modeRaw of
      "cue" -> pure SongModeCue
      "drone" -> pure SongModeDrone
      _ -> Left ("invalid song.mode: " <> modeRaw <> " (expected cue|drone)")
  sections <- parseSections mood entries
  ensure (not (null sections)) "song must define at least one section under sections.<name>"
  let sectionNames = M.keys sections
  instruments0 <- parseInstruments sectionNames entries
  instruments <-
    if null instruments0
      then arrangeGeneratedInstruments genre patchOverrides sections
      else pure instruments0
  ensure (not (null instruments)) "song must define at least one instrument under instruments.<name>"
  pure
    SongSpec
      { sgMode = mode
      , sgGenre = genre
      , sgMood = mood
      , sgForm = form
      , sgLookaheadBars = lookaheadBars
      , sgInstruments = instruments
      , sgSections = sections
      }

validateSongGenre ∷ String → Either String String
validateSongGenre genreRaw = do
  let genre = normalizeName genreRaw
  ensure (isSupportedGenre genre) ("unsupported song.genre: " <> genreRaw <> " (expected electronic|ambient|blackmetal|cinematic)")
  pure genre


parseSections ∷ String → FlatYaml → Either String (Map String SectionSpec)
parseSections songMood entries = do
  topTempo <- lookupMaybeFloatKey ["song", "tempo_bpm"] entries
  topBeats <- lookupMaybeIntKey ["song", "beats_per_bar"] entries
  topBeatUnit <- lookupMaybeIntKey ["song", "beat_unit"] entries
  let sectionNames =
        nub
          [ normalizeName name
          | path <- M.keys entries
          , Just ("sections", name) <- [prefix2 path]
          ]
  pairs <- mapM (parseSection topTempo topBeats topBeatUnit) sectionNames
  pure (M.fromList pairs)
  where
    parseSection topTempo topBeats topBeatUnit name = do
      tempo <- lookupFloatKeyMaybeDefault ["sections", name, "tempo_bpm"] topTempo entries
      beats <- lookupIntKeyMaybeDefault ["sections", name, "beats_per_bar"] topBeats entries
      beatUnit <- lookupIntKeyMaybeDefault ["sections", name, "beat_unit"] (Just (fromMaybe 4 topBeatUnit)) entries
      barsPerPhrase <- lookupIntKeyDefault ["sections", name, "bars_per_phrase"] 1 entries
      phraseCount <- lookupIntKeyDefault ["sections", name, "phrase_count"] 1 entries
      let mood = lookupTextDefault ["sections", name, "mood"] songMood entries
          feel = lookupTextDefault ["sections", name, "feel"] "" entries
          chordRaw = lookupTextDefault ["sections", name, "chords"] "" entries
          melodyRaw = lookupTextDefault ["sections", name, "melody"] "" entries
      chordBars <- parseChordProgression name chordRaw
      chordRegions <- parseChordRegions name beats barsPerPhrase entries
      melodyPhrase <- parsePatternNotes name melodyRaw
      ensure (tempo > 0) ("sections." <> name <> ".tempo_bpm must be > 0")
      ensure (beats > 0) ("sections." <> name <> ".beats_per_bar must be > 0")
      ensure (beatUnit > 0) ("sections." <> name <> ".beat_unit must be > 0")
      ensure (barsPerPhrase > 0) ("sections." <> name <> ".bars_per_phrase must be > 0")
      ensure (phraseCount > 0) ("sections." <> name <> ".phrase_count must be > 0")
      pure
        ( name
        , SectionSpec
            { ssName = name
            , ssTempoBpm = tempo
            , ssBeatsPerBar = beats
            , ssBeatUnit = beatUnit
            , ssBarsPerPhrase = barsPerPhrase
            , ssPhraseCount = phraseCount
            , ssMood = mood
            , ssFeel = feel
            , ssChordBars = chordBars
            , ssChordRegions = chordRegions
            , ssMelodyPhrase = melodyPhrase
            }
        )

parseInstruments ∷ [String] → FlatYaml → Either String [InstrumentPatternSpec]
parseInstruments sectionNames entries = do
  let instNames =
        nub
          [ name
          | path <- M.keys entries
          , Just ("instruments", name) <- [prefix2 path]
          ]
  mapM parseInstrument instNames
  where
    parseInstrument name = do
      iid <- lookupIntKey ["instruments", name, "instrument_id"] entries
      let patchPath = lookupMaybePatchPath ["instruments", name] entries
      amp <- lookupFloatKeyDefault ["instruments", name, "amp"] 1 entries
      pan <- lookupFloatKeyDefault ["instruments", name, "pan"] 0 entries
      velocityVariation <- lookupFloatKeyDefault ["instruments", name, "velocity_variation"] 0 entries
      fillGenerator <- parseFillGenerator name
      ensure (iid >= 0) ("instruments." <> name <> ".instrument_id must be >= 0")
      ensure
        (velocityVariation >= 0 && velocityVariation <= 1)
        ("instruments." <> name <> ".velocity_variation must be in [0,1]")
      patterns <- mapM (parseSectionPattern name) sectionNames
      fills <- mapM (parseSectionFillPattern name) sectionNames
      pure
        InstrumentPatternSpec
          { ipName = name
          , ipInstrumentId = InstrumentId iid
          , ipPatchPath = patchPath
          , ipAmp = amp
          , ipPan = pan
          , ipVelocityVariation = velocityVariation
          , ipPatterns = M.fromList patterns
          , ipFills = M.fromList fills
          , ipFillGenerator = fillGenerator
          }

    parseFillGenerator instName = do
      let basePath = ["instruments", instName, "fill_generator"]
          blockPresent = any (pathStartsWith basePath) (M.keys entries)
      enabled <- lookupBoolKeyDefault (basePath <> ["enabled"]) blockPresent entries
      if not enabled
        then pure Nothing
        else do
          variation <- lookupFloatKeyDefault (basePath <> ["variation"]) 0.65 entries
          density <- lookupFloatKeyDefault (basePath <> ["density"]) 0.72 entries
          lengthBeats <- lookupFloatKeyDefault (basePath <> ["length_beats"]) 2.0 entries
          ensure
            (variation >= 0 && variation <= 1)
            ("instruments." <> instName <> ".fill_generator.variation must be in [0,1]")
          ensure
            (density >= 0 && density <= 1)
            ("instruments." <> instName <> ".fill_generator.density must be in [0,1]")
          ensure
            (lengthBeats > 0)
            ("instruments." <> instName <> ".fill_generator.length_beats must be > 0")
          pure
            (Just
               FillGeneratorSpec
                 { fgVariation = variation
                 , fgDensity = density
                 , fgLengthBeats = lengthBeats
                 })

    parseSectionPattern instName sectionName = do
      let dottedPath = ["instruments", instName, "patterns", sectionName]
          prefixedPath = ["instruments", instName, "pattern_" <> sectionName]
          raw =
            case M.lookup dottedPath entries of
              Just txt -> txt
              Nothing -> lookupTextDefault prefixedPath "" entries
      notes <- parsePatternNotes sectionName raw
      pure (sectionName, notes)

    parseSectionFillPattern instName sectionName = do
      let dottedPath = ["instruments", instName, "fills", sectionName]
          prefixedPath = ["instruments", instName, "fill_" <> sectionName]
          raw =
            case M.lookup dottedPath entries of
              Just txt -> txt
              Nothing -> lookupTextDefault prefixedPath "" entries
      notes <- parsePatternNotes sectionName raw
      pure (sectionName, notes)

arrangeGeneratedInstruments ∷ String → Map String FilePath → Map String SectionSpec → Either String [InstrumentPatternSpec]
arrangeGeneratedInstruments genre patchOverrides sections
  | not (any sectionNeedsArrangement (M.elems sections)) = pure []
  | otherwise = generatedInstrumentSpecsWithPatches genre patchOverrides
  where
    sectionNeedsArrangement spec =
      not (null (ssChordBars spec)) || not (null (ssMelodyPhrase spec))
        || not (null (ssChordRegions spec))

generatedInstrumentSpecs ∷ String → Either String [InstrumentPatternSpec]
generatedInstrumentSpecs genre = generatedInstrumentSpecsWithPatches genre M.empty

generatedInstrumentSpecsWithPatches ∷ String → Map String FilePath → Either String [InstrumentPatternSpec]
generatedInstrumentSpecsWithPatches genre patchOverrides
  | null genre = Left "song.genre is required when using section chords/melody without explicit instruments"
  | normalizeName genre == "electronic" =
      pure
        [ generatedInstrument patchOverrides "drums" 9 1.0 0 0.7 (Just (FillGeneratorSpec 0.62 0.74 2.0))
        , generatedInstrument patchOverrides "bass" 38 0.88 0 0.22 Nothing
        , generatedInstrument patchOverrides "pad" 88 0.52 0 0.10 Nothing
        , generatedInstrument patchOverrides "arp" 81 0.46 0.12 0.18 Nothing
        , generatedInstrument patchOverrides "lead" 80 0.86 (-0.05) 0.12 Nothing
        ]
  | normalizeName genre == "ambient" =
      pure
        [ generatedInstrument patchOverrides "drums" 9 0.76 0 0.24 (Just (FillGeneratorSpec 0.30 0.24 1.0))
        , generatedInstrument patchOverrides "bass" 39 0.54 0 0.10 Nothing
        , generatedInstrument patchOverrides "pad" 94 0.68 0 0.04 Nothing
        , generatedInstrument patchOverrides "arp" 98 0.32 0.10 0.06 Nothing
        , generatedInstrument patchOverrides "lead" 88 0.38 0 0.04 Nothing
        ]
  | normalizeName genre == "blackmetal" =
      pure
        [ generatedInstrument patchOverrides "drums" 9 1.0 0 0.58 (Just (FillGeneratorSpec 0.78 0.84 2.0))
        , generatedInstrument patchOverrides "bass" 34 0.92 0 0.18 Nothing
        , generatedInstrument patchOverrides "pad" 48 0.58 0 0.08 Nothing
        , generatedInstrument patchOverrides "arp" 30 0.84 (-0.08) 0.14 Nothing
        , generatedInstrument patchOverrides "lead" 31 0.90 0.08 0.12 Nothing
        ]
  | normalizeName genre == "cinematic" =
      pure
        [ generatedInstrument patchOverrides "drums" 9 0.96 0 0.38 (Just (FillGeneratorSpec 0.48 0.54 1.5))
        , generatedInstrument patchOverrides "bass" 43 0.82 0 0.12 Nothing
        , generatedInstrument patchOverrides "pad" 91 0.62 0 0.06 Nothing
        , generatedInstrument patchOverrides "arp" 48 0.44 0.08 0.10 Nothing
        , generatedInstrument patchOverrides "lead" 61 0.78 0 0.08 Nothing
        ]
  | otherwise = Left ("unsupported song.genre: " <> genre <> " (expected electronic|ambient|blackmetal|cinematic)")

lookupGeneratedInstrument ∷ String → String → Either String InstrumentPatternSpec
lookupGeneratedInstrument genre instrumentName = do
  instruments <- generatedInstrumentSpecs genre
  case find ((== normalizeName instrumentName) . normalizeName . ipName) instruments of
    Just instrumentSpec -> pure instrumentSpec
    Nothing ->
      Left
        ( "unsupported generated instrument `" <> instrumentName <> "` for genre "
            <> normalizeName genre
            <> " (expected one of: "
            <> unwords (map ipName instruments)
            <> ")"
        )

generatedInstrument ∷ Map String FilePath → String → Int → Float → Float → Float → Maybe FillGeneratorSpec → InstrumentPatternSpec
generatedInstrument patchOverrides name iid amp pan velocityVariation fillGenerator =
  InstrumentPatternSpec
    { ipName = name
    , ipInstrumentId = InstrumentId iid
    , ipPatchPath = M.lookup (normalizeName name) patchOverrides
    , ipAmp = amp
    , ipPan = pan
    , ipVelocityVariation = velocityVariation
    , ipPatterns = M.empty
    , ipFills = M.empty
    , ipFillGenerator = fillGenerator
    }

parseGeneratedPatchOverrides ∷ FlatYaml → Map String FilePath
parseGeneratedPatchOverrides entries =
  M.fromList
    [ (normalizeName role, patchPath)
    | path <- M.keys entries
    , Just role <- [songPatchRole path]
    , Just patchPath <- [lookupMaybeGeneratedPatchPath role entries]
    ]

songPatchRole ∷ [String] → Maybe String
songPatchRole path =
  case path of
    ["song", "patches", role] -> Just role
    ["song", "patches", role, "patch"] -> Just role
    ["song", "patches", role, "patch_file"] -> Just role
    _ -> Nothing

lookupMaybePatchPath ∷ [String] → FlatYaml → Maybe FilePath
lookupMaybePatchPath basePath entries =
  case nonEmptyText (lookupTextDefault (basePath <> ["patch"]) "" entries) of
    Just patchPath -> Just patchPath
    Nothing -> nonEmptyText (lookupTextDefault (basePath <> ["patch_file"]) "" entries)

lookupMaybeGeneratedPatchPath ∷ String → FlatYaml → Maybe FilePath
lookupMaybeGeneratedPatchPath role entries =
  case nonEmptyText (lookupTextDefault ["song", "patches", role] "" entries) of
    Just patchPath -> Just patchPath
    Nothing -> lookupMaybePatchPath ["song", "patches", role] entries

instrumentPatchOverrides ∷ [InstrumentPatternSpec] → Map String FilePath
instrumentPatchOverrides instruments =
  M.fromList
    [ (normalizeName (ipName inst), patchPath)
    | inst <- instruments
    , Just patchPath <- [ipPatchPath inst]
    ]

resolveSongSpecPatchPaths ∷ FilePath → SongSpec → Either String SongSpec
resolveSongSpecPatchPaths baseDir spec =
  pure
    spec
      { sgInstruments = map (resolveInstrumentPatchPath baseDir) (sgInstruments spec)
      }

resolveInstrumentPatchPath ∷ FilePath → InstrumentPatternSpec → InstrumentPatternSpec
resolveInstrumentPatchPath baseDir inst =
  inst
    { ipPatchPath = fmap (resolvePatchPath baseDir) (ipPatchPath inst)
    }

resolvePatchPath ∷ FilePath → FilePath → FilePath
resolvePatchPath baseDir rawPath =
  let path = trim rawPath
  in if null path
       then path
       else
         normalise
           ( if isAbsolute path
               then path
               else baseDir </> path
           )

isSupportedGenre ∷ String → Bool
isSupportedGenre genre =
  let g = normalizeName genre
  in g == "electronic" || g == "ambient" || g == "blackmetal" || g == "cinematic"

parsePatternNotes ∷ String → String → Either String [PatternNote]
parsePatternNotes _sectionName rawPattern =
  let trimmed = trim rawPattern
  in if null trimmed
       then pure []
       else sortOn pnBeatOffset <$> mapM parseToken (splitOn ',' trimmed)
  where
    parseToken token = do
      let parts = map trim (splitOn '/' token)
      ensure (length parts == 4) ("invalid note token " <> show token <> " (expected beat/key/duration/velocity)")
      case parts of
        [beatTxt, keyTxt, durTxt, velTxt] -> do
          beat <- parseFloat "beat" beatTxt
          key <- parseNoteKey "key" keyTxt
          dur <- parseFloat "duration" durTxt
          vel <- parseFloat "velocity" velTxt
          ensure (beat >= 0) ("note beat offset must be >= 0 in token " <> show token)
          ensure (key >= 0 && key <= 127) ("note key must be in [0,127] in token " <> show token)
          ensure (dur > 0) ("note duration must be > 0 in token " <> show token)
          pure
            PatternNote
              { pnBeatOffset = beat
              , pnNoteKey = NoteKey key
              , pnDuration = dur
              , pnVelocity = max 0 (min 1 vel)
              }
        _ ->
          Left ("invalid note token " <> show token <> " (expected beat/key/duration/velocity)")

parseChordProgression ∷ String → String → Either String [ChordSpec]
parseChordProgression _sectionName rawChords =
  let tokens = filter (not . null) (map trim (splitOn ',' rawChords))
  in mapM parseChordSymbol tokens

parseChordRegions ∷ String → Int → Int → FlatYaml → Either String [ChordRegionSpec]
parseChordRegions sectionName beatsPerBar barsPerPhrase entries = do
  let regionNames =
        nub
          [ regionName
          | path <- M.keys entries
          , Just ("sections", name, "chord_regions", regionName) <- [prefix4 path]
          , name == sectionName
          ]
  regions <- sortOn crStartBeat <$> mapM parseRegion regionNames
  case regions of
    [] -> pure []
    firstRegion : _ -> do
      ensure (abs (crStartBeat firstRegion) < 0.0001) ("sections." <> sectionName <> ".chord_regions must start at beat 0")
      pure regions
  where
    phraseBeats = fromIntegral (max 1 beatsPerBar * max 1 barsPerPhrase)
    parseRegion regionName = do
      let basePath = ["sections", sectionName, "chord_regions", regionName]
          symbol =
            let fromSymbol = lookupTextDefault (basePath <> ["symbol"]) "" entries
            in if null fromSymbol
                 then lookupTextDefault (basePath <> ["chord"]) "" entries
                 else fromSymbol
      ensure (not (null symbol)) ("missing key " <> renderPath (basePath <> ["symbol"]))
      startBeat <- lookupFloatKey (basePath <> ["start_beat"]) entries
      ensure (startBeat >= 0) ("sections." <> sectionName <> ".chord_regions." <> regionName <> ".start_beat must be >= 0")
      ensure (startBeat < phraseBeats) ("sections." <> sectionName <> ".chord_regions." <> regionName <> ".start_beat must be inside the phrase")
      chord <- parseChordSymbol symbol
      pure
        ChordRegionSpec
          { crStartBeat = startBeat
          , crChord = chord
          }

type FlatYaml = Map [String] String

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
              path = parent <> [normalizeName key]
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

prefix2 ∷ [a] → Maybe (a, a)
prefix2 (a : b : _) = Just (a, b)
prefix2 _ = Nothing

prefix4 ∷ [a] → Maybe (a, a, a, a)
prefix4 (a : b : c : d : _) = Just (a, b, c, d)
prefix4 _ = Nothing

lookupIntKey ∷ [String] → FlatYaml → Either String Int
lookupIntKey key entries =
  case M.lookup key entries of
    Nothing -> Left ("missing key " <> renderPath key)
    Just raw ->
      case reads raw of
        [(n, "")] -> pure n
        _ -> Left ("invalid integer for " <> renderPath key <> ": " <> raw)

lookupFloatKey ∷ [String] → FlatYaml → Either String Float
lookupFloatKey key entries =
  case M.lookup key entries of
    Nothing -> Left ("missing key " <> renderPath key)
    Just raw ->
      case reads raw of
        [(n, "")] -> pure n
        _ -> Left ("invalid float for " <> renderPath key <> ": " <> raw)

lookupIntKeyDefault ∷ [String] → Int → FlatYaml → Either String Int
lookupIntKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupIntKey key entries

lookupFloatKeyDefault ∷ [String] → Float → FlatYaml → Either String Float
lookupFloatKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupFloatKey key entries

lookupMaybeIntKey ∷ [String] → FlatYaml → Either String (Maybe Int)
lookupMaybeIntKey key entries =
  case M.lookup key entries of
    Nothing -> pure Nothing
    Just _ -> Just <$> lookupIntKey key entries

lookupMaybeFloatKey ∷ [String] → FlatYaml → Either String (Maybe Float)
lookupMaybeFloatKey key entries =
  case M.lookup key entries of
    Nothing -> pure Nothing
    Just _ -> Just <$> lookupFloatKey key entries

lookupIntKeyMaybeDefault ∷ [String] → Maybe Int → FlatYaml → Either String Int
lookupIntKeyMaybeDefault key fallback entries =
  case M.lookup key entries of
    Just _ -> lookupIntKey key entries
    Nothing ->
      case fallback of
        Just value -> pure value
        Nothing -> Left ("missing key " <> renderPath key)

lookupFloatKeyMaybeDefault ∷ [String] → Maybe Float → FlatYaml → Either String Float
lookupFloatKeyMaybeDefault key fallback entries =
  case M.lookup key entries of
    Just _ -> lookupFloatKey key entries
    Nothing ->
      case fallback of
        Just value -> pure value
        Nothing -> Left ("missing key " <> renderPath key)

lookupTextDefault ∷ [String] → String → FlatYaml → String
lookupTextDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> fallback
    Just v -> v

lookupBoolKey ∷ [String] → FlatYaml → Either String Bool
lookupBoolKey key entries =
  case M.lookup key entries of
    Nothing -> Left ("missing key " <> renderPath key)
    Just raw ->
      case map toLower raw of
        "true" -> pure True
        "false" -> pure False
        "yes" -> pure True
        "no" -> pure False
        "on" -> pure True
        "off" -> pure False
        _ -> Left ("invalid bool for " <> renderPath key <> ": " <> raw)

lookupBoolKeyDefault ∷ [String] → Bool → FlatYaml → Either String Bool
lookupBoolKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupBoolKey key entries

parseFloat ∷ String → String → Either String Float
parseFloat label raw =
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid " <> label <> ": " <> raw)

parseNoteKey ∷ String → String → Either String Int
parseNoteKey label raw =
  case reads raw of
    [(n, "")] -> pure n
    _ ->
      case parseMidiNoteName raw of
        Just noteKey -> pure noteKey
        Nothing -> Left ("invalid " <> label <> ": " <> raw)

parseMidiNoteName ∷ String → Maybe Int
parseMidiNoteName raw =
  case trim raw of
    letter : rest
      | isAlpha letter ->
          let rootPc = case toLower letter of
                'c' -> Just 0
                'd' -> Just 2
                'e' -> Just 4
                'f' -> Just 5
                'g' -> Just 7
                'a' -> Just 9
                'b' -> Just 11
                _ -> Nothing
              (accidental, octaveTxt) =
                case rest of
                  '#' : xs -> (1, xs)
                  'b' : xs -> (-1, xs)
                  _ -> (0, rest)
          in do
            pc0 <- rootPc
            ensureMaybe (not (null octaveTxt) && all (\c -> isDigit c || c == '-') octaveTxt)
            octave <- readMaybeInt octaveTxt
            let midi = 12 * (octave + 1) + ((pc0 + accidental) `mod` 12)
            ensureMaybe (midi >= 0 && midi <= 127)
            pure midi
    _ -> Nothing

parseChordSymbol ∷ String → Either String ChordSpec
parseChordSymbol raw =
  case trim raw of
    [] -> Left "chord token must not be empty"
    rootLetter : rest
      | isAlpha rootLetter ->
          let rootPc = case toLower rootLetter of
                'c' -> Just 0
                'd' -> Just 2
                'e' -> Just 4
                'f' -> Just 5
                'g' -> Just 7
                'a' -> Just 9
                'b' -> Just 11
                _ -> Nothing
              (accidental, suffixRaw) =
                case rest of
                  '#' : xs -> (1, xs)
                  'b' : xs -> (-1, xs)
                  _ -> (0, rest)
          in case rootPc of
               Nothing -> Left ("invalid chord root: " <> raw)
               Just pc0 ->
                 let suffix = normalizeChordSuffix suffixRaw
                     minorish = startsWith "m" suffix && not (startsWith "maj" suffix)
                     baseTriad
                       | "sus2" `containsText` suffix = [0, 2, 7]
                       | "sus4" `containsText` suffix = [0, 5, 7]
                       | "dim" `containsText` suffix = [0, 3, 6]
                       | "aug" `containsText` suffix = [0, 4, 8]
                       | minorish = [0, 3, 7]
                       | otherwise = [0, 4, 7]
                     seventh
                       | "maj7" `containsText` suffix = [11]
                       | "m7" `containsText` suffix = [10]
                       | "7" `containsText` suffix = [10]
                       | otherwise = []
                     ninth
                       | "add9" `containsText` suffix = [14]
                       | "maj9" `containsText` suffix = [14]
                       | startsWith "m9" suffix = [14]
                       | suffix == "9" = [14]
                       | otherwise = []
                 in pure
                      ChordSpec
                        { csSymbol = trim raw
                        , csRootClass = (pc0 + accidental) `mod` 12
                        , csIntervals = nub (baseTriad <> seventh <> ninth)
                        }
      | otherwise -> Left ("invalid chord root: " <> raw)

normalizeChordSuffix ∷ String → String
normalizeChordSuffix = map toLower . filter (\c -> isAlphaNum c || c == '#')

containsText ∷ String → String → Bool
containsText needle haystack = needle `isInfixOf` haystack

startsWith ∷ String → String → Bool
startsWith prefix txt = take (length prefix) txt == prefix

splitOn ∷ Char → String → [String]
splitOn _ "" = [""]
splitOn delim xs = go xs ""
  where
    go [] acc = [reverse acc]
    go (c : cs) acc
      | c == delim = reverse acc : go cs ""
      | otherwise = go cs (c : acc)

stripComment ∷ String → String
stripComment = reverse . dropWhile isSpace . reverse . takeWhile (/= '#')

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse

normalizeName ∷ String → String
normalizeName = map toLower . trim

parseNameList ∷ String → [String]
parseNameList raw = filter (not . null) (map normalizeSectionName (splitOn ',' raw))

nonEmptyText ∷ String → Maybe String
nonEmptyText raw =
  let stripped = trim raw
  in if null stripped then Nothing else Just stripped

ensureMaybe ∷ Bool → Maybe ()
ensureMaybe ok = if ok then Just () else Nothing

readMaybeInt ∷ String → Maybe Int
readMaybeInt raw =
  case reads raw of
    [(n, "")] -> Just n
    _ -> Nothing

pathStartsWith ∷ Eq a ⇒ [a] → [a] → Bool
pathStartsWith prefix xs = take (length prefix) xs == prefix

renderPath ∷ [String] → String
renderPath = foldl render ""
  where
    render "" seg = seg
    render acc seg = acc <> "." <> seg

ensure ∷ Bool → String → Either String ()
ensure cond msg =
  if cond then pure () else Left msg


normalizeSectionName ∷ String → String
normalizeSectionName = map toLower . trim
