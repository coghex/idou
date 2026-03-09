{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline
  ( SongMode(..)
  , PatternNote(..)
  , InstrumentPatternSpec(..)
  , SectionSpec(..)
  , SongSpec(..)
  , TimelineNote(..)
  , TimelineBar(..)
  , TimelineTransitionTelemetry(..)
  , TimelineRuntime(..)
  , loadSongSpec
  , parseSongSpecText
  , cueSectionOrder
  , compileTimelineBars
  , prepareTimelineRuntime
  , setTimelineTargets
  , popReadyBars
  , popTransitionTelemetry
  , timelineRuntimeDone
  ) where

import Data.Char (isSpace, toLower)
import Data.Bits (xor)
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Word (Word32, Word64)

import qualified Data.Map.Strict as M

import Audio.Types (InstrumentId(..), NoteKey(..))

data SongMode
  = SongModeCue
  | SongModeDrone
  deriving (Eq, Show)

data PatternNote = PatternNote
  { pnBeatOffset ∷ !Float
  , pnNoteKey    ∷ !NoteKey
  , pnDuration   ∷ !Float
  , pnVelocity   ∷ !Float
  }
  deriving (Eq, Show)

data InstrumentPatternSpec = InstrumentPatternSpec
  { ipName        ∷ !String
  , ipInstrumentId ∷ !InstrumentId
  , ipAmp         ∷ !Float
  , ipPan         ∷ !Float
  , ipPatterns    ∷ !(Map String [PatternNote])
  }
  deriving (Eq, Show)

data SectionSpec = SectionSpec
  { ssName          ∷ !String
  , ssTempoBpm      ∷ !Float
  , ssBeatsPerBar   ∷ !Int
  , ssBeatUnit      ∷ !Int
  , ssBarsPerPhrase ∷ !Int
  , ssPhraseCount   ∷ !Int
  , ssMood          ∷ !String
  , ssFeel          ∷ !String
  }
  deriving (Eq, Show)

data SongSpec = SongSpec
  { sgMode          ∷ !SongMode
  , sgLookaheadBars ∷ !Int
  , sgInstruments   ∷ ![InstrumentPatternSpec]
  , sgSections      ∷ !(Map String SectionSpec)
  }
  deriving (Eq, Show)

data TimelineNote = TimelineNote
  { tnInstrumentId     ∷ !InstrumentId
  , tnAmp              ∷ !Float
  , tnPan              ∷ !Float
  , tnKey              ∷ !NoteKey
  , tnVelocity         ∷ !Float
  , tnOnOffsetFrames   ∷ !Word64
  , tnOffOffsetFrames  ∷ !Word64
  }
  deriving (Eq, Show)

data TimelineBar = TimelineBar
  { tbSectionName      ∷ !String
  , tbAbsoluteBarIx    ∷ !Int
  , tbPhraseIx         ∷ !Int
  , tbBarInPhrase      ∷ !Int
  , tbTempoBpm         ∷ !Float
  , tbBeatsPerBar      ∷ !Int
  , tbBeatUnit         ∷ !Int
  , tbStartOffsetFrames ∷ !Word64
  , tbLengthFrames     ∷ !Word64
  , tbNotes            ∷ ![TimelineNote]
  }
  deriving (Eq, Show)

data TimelineTransitionTelemetry = TimelineTransitionTelemetry
  { ttFromSectionName     ∷ !String
  , ttToSectionName       ∷ !String
  , ttReason              ∷ !String
  , ttBoundaryBarIx       ∷ !Int
  , ttBoundaryOffsetFrames ∷ !Word64
  , ttBaseWeights         ∷ ![(String, Int)]
  , ttFinalWeights        ∷ ![(String, Int)]
  , ttMoodTarget          ∷ !(Maybe String)
  , ttEnergyTarget        ∷ !Float
  , ttPickTicket          ∷ !Int
  , ttPickTotal           ∷ !Int
  }
  deriving (Eq, Show)

data TimelineRuntime = TimelineRuntime
  { trBars               ∷ ![TimelineBar]
  , trStartFrame         ∷ !Word64
  , trLookaheadBars      ∷ !Int
  , trNextBarIx          ∷ !Int
  , trSampleRateHz       ∷ !Word32
  , trMode               ∷ !SongMode
  , trSections           ∷ !(Map String SectionSpec)
  , trInstruments        ∷ ![InstrumentPatternSpec]
  , trCurrentSectionName ∷ !String
  , trCurrentSectionBar  ∷ !Int
  , trGeneratedBarCount  ∷ !Int
  , trNextBarOffset      ∷ !Word64
  , trRngState           ∷ !Word64
  , trTransitionCount    ∷ !Int
  , trTargetMood         ∷ !(Maybe String)
  , trTargetEnergy       ∷ !Float
  , trPendingTransitions ∷ ![TimelineTransitionTelemetry]
  , trDone               ∷ !Bool
  }
  deriving (Eq, Show)

loadSongSpec ∷ FilePath → IO (Either String SongSpec)
loadSongSpec path = do
  text <- readFile path
  pure (parseSongSpecText text)

parseSongSpecText ∷ String → Either String SongSpec
parseSongSpecText contents = do
  entries <- parseFlatYaml contents
  let modeRaw = map toLower (lookupTextDefault ["song", "mode"] "cue" entries)
  lookaheadBars <- max 1 <$> lookupIntKeyDefault ["song", "lookahead_bars"] 2 entries
  mode <-
    case modeRaw of
      "cue" -> pure SongModeCue
      "drone" -> pure SongModeDrone
      _ -> Left ("invalid song.mode: " <> modeRaw <> " (expected cue|drone)")
  sections <- parseSections entries
  ensure (not (null sections)) "song must define at least one section under sections.<name>"
  let sectionNames = M.keys sections
  instruments <- parseInstruments sectionNames entries
  ensure (not (null instruments)) "song must define at least one instrument under instruments.<name>"
  pure
    SongSpec
      { sgMode = mode
      , sgLookaheadBars = lookaheadBars
      , sgInstruments = instruments
      , sgSections = sections
      }

cueSectionOrder ∷ SongSpec → [String]
cueSectionOrder spec =
  let available = M.keys (sgSections spec)
      has name = name `elem` available
      cueTemplate = ["intro", "verse", "chorus", "verse", "bridge", "chorus", "ending"]
      droneTemplate = ["intro", "verse", "bridge", "chorus", "verse", "bridge", "chorus", "ending"]
      template =
        case sgMode spec of
          SongModeCue -> cueTemplate
          SongModeDrone -> droneTemplate
      planned = filter has template
      canonUnique = nub (cueTemplate <> droneTemplate)
      extras = filter (`notElem` canonUnique) available
  in if null planned then available else planned <> extras

compileTimelineBars ∷ Word32 → SongSpec → [TimelineBar]
compileTimelineBars sampleRateHz spec = reverse barsRev
  where
    (barsRev, _offset, _absIx) =
      foldl
        compileSection
        ([], 0, 0)
        (cueSectionOrder spec)

    compileSection (acc, offsetFrames, absIx) sectionName =
      case M.lookup sectionName (sgSections spec) of
        Nothing -> (acc, offsetFrames, absIx)
        Just sectionSpec ->
          let barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
              phraseCount = max 1 (ssPhraseCount sectionSpec)
              barCount = barsPerPhrase * phraseCount
              barFrames = sectionBarFrames sampleRateHz sectionSpec
              notes = compileSectionNotes sampleRateHz sectionName sectionSpec (sgInstruments spec)
              buildBar i =
                let i64 = fromIntegral i ∷ Word64
                    absIx' = absIx + i
                in TimelineBar
                    { tbSectionName = sectionName
                    , tbAbsoluteBarIx = absIx'
                    , tbPhraseIx = i `div` barsPerPhrase
                    , tbBarInPhrase = i `mod` barsPerPhrase
                    , tbTempoBpm = ssTempoBpm sectionSpec
                    , tbBeatsPerBar = ssBeatsPerBar sectionSpec
                    , tbBeatUnit = ssBeatUnit sectionSpec
                    , tbStartOffsetFrames = offsetFrames + i64 * barFrames
                    , tbLengthFrames = barFrames
                    , tbNotes = notes
                    }
              newBars = map buildBar [0 .. barCount - 1]
              nextOffset = offsetFrames + fromIntegral barCount * barFrames
          in (reverse newBars <> acc, nextOffset, absIx + barCount)

prepareTimelineRuntime ∷ Word32 → Word64 → SongSpec → TimelineRuntime
prepareTimelineRuntime sampleRateHz startFrame spec =
  let firstSection = initialSectionName spec
      seed0 = startFrame * 0x9E3779B97F4A7C15 + fromIntegral (length (sgSections spec))
  in TimelineRuntime
      { trBars = []
      , trStartFrame = startFrame
      , trLookaheadBars = max 1 (sgLookaheadBars spec)
      , trNextBarIx = 0
      , trSampleRateHz = sampleRateHz
      , trMode = sgMode spec
      , trSections = sgSections spec
      , trInstruments = sgInstruments spec
      , trCurrentSectionName = firstSection
      , trCurrentSectionBar = 0
      , trGeneratedBarCount = 0
      , trNextBarOffset = 0
      , trRngState = seed0
       , trTransitionCount = 0
       , trTargetMood = Nothing
       , trTargetEnergy = 0.5
       , trPendingTransitions = []
       , trDone = M.null (sgSections spec)
       }

setTimelineTargets ∷ Maybe String → Float → TimelineRuntime → TimelineRuntime
setTimelineTargets mood energy rt =
  rt
    { trTargetMood = normalizeMood mood
    , trTargetEnergy = clamp01 energy
    }

popReadyBars ∷ Word64 → TimelineRuntime → ([TimelineBar], TimelineRuntime)
popReadyBars now runtime = go [] (ensureGeneratedToHorizon now runtime)
  where
    go acc rt =
      case nextBar rt of
        Nothing -> (reverse acc, rt)
        Just bar ->
          let barStartAbs = trStartFrame rt + tbStartOffsetFrames bar
              lookaheadFrames = fromIntegral (trLookaheadBars rt) * tbLengthFrames bar
              horizon = now + lookaheadFrames
          in if barStartAbs <= horizon
               then
                 go
                   (bar : acc)
                   rt { trNextBarIx = trNextBarIx rt + 1 }
               else
                 (reverse acc, rt)

timelineRuntimeDone ∷ TimelineRuntime → Bool
timelineRuntimeDone rt = trDone rt && trNextBarIx rt >= length (trBars rt)

popTransitionTelemetry ∷ TimelineRuntime → ([TimelineTransitionTelemetry], TimelineRuntime)
popTransitionTelemetry rt =
  (reverse (trPendingTransitions rt), rt { trPendingTransitions = [] })

ensureGeneratedToHorizon ∷ Word64 → TimelineRuntime → TimelineRuntime
ensureGeneratedToHorizon now runtime = go runtime
  where
    go rt
      | trDone rt = rt
      | trStartFrame rt + trNextBarOffset rt > generationHorizon now rt = rt
      | otherwise = go (appendGeneratedBar rt)

generationHorizon ∷ Word64 → TimelineRuntime → Word64
generationHorizon now rt =
  let barFrames = currentSectionBarFrames rt
  in now + fromIntegral (trLookaheadBars rt) * barFrames

currentSectionBarFrames ∷ TimelineRuntime → Word64
currentSectionBarFrames rt =
  case M.lookup (trCurrentSectionName rt) (trSections rt) of
    Nothing -> 1
    Just sectionSpec -> sectionBarFrames (trSampleRateHz rt) sectionSpec

appendGeneratedBar ∷ TimelineRuntime → TimelineRuntime
appendGeneratedBar rt =
  case M.lookup (trCurrentSectionName rt) (trSections rt) of
    Nothing ->
      rt { trDone = True }
    Just sectionSpec ->
      let barFrames = sectionBarFrames (trSampleRateHz rt) sectionSpec
          barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
          phraseCount = max 1 (ssPhraseCount sectionSpec)
          sectionBarCount = barsPerPhrase * phraseCount
          barIx = trCurrentSectionBar rt
          (variedNotes, nextBarSeed) =
            varyBarNotes
              (trMode rt)
              (trTargetEnergy rt)
              (trTargetMood rt)
              sectionSpec
              (trGeneratedBarCount rt)
              (trRngState rt)
              (compileSectionNotes (trSampleRateHz rt) (trCurrentSectionName rt) sectionSpec (trInstruments rt))
          bar =
            TimelineBar
              { tbSectionName = trCurrentSectionName rt
              , tbAbsoluteBarIx = trGeneratedBarCount rt
              , tbPhraseIx = barIx `div` barsPerPhrase
              , tbBarInPhrase = barIx `mod` barsPerPhrase
              , tbTempoBpm = ssTempoBpm sectionSpec
              , tbBeatsPerBar = ssBeatsPerBar sectionSpec
              , tbBeatUnit = ssBeatUnit sectionSpec
              , tbStartOffsetFrames = trNextBarOffset rt
              , tbLengthFrames = barFrames
              , tbNotes = variedNotes
              }
          nextOffset = trNextBarOffset rt + barFrames
          nextBarInSection = barIx + 1
          phraseBoundary = nextBarInSection `mod` barsPerPhrase == 0
          finishedSection = phraseBoundary && nextBarInSection >= sectionBarCount
          rtBase =
            rt
              { trBars = trBars rt <> [bar]
              , trGeneratedBarCount = trGeneratedBarCount rt + 1
              , trNextBarOffset = nextOffset
              , trRngState = nextBarSeed
              }
      in if not finishedSection
           then rtBase { trCurrentSectionBar = nextBarInSection }
           else advanceSection rtBase

advanceSection ∷ TimelineRuntime → TimelineRuntime
advanceSection rt
  | trMode rt == SongModeCue && trCurrentSectionName rt == "ending" =
      rt { trDone = True, trCurrentSectionBar = 0 }
  | otherwise =
      case pickNextSection rt of
        Nothing ->
          case trMode rt of
            SongModeCue -> rt { trDone = True, trCurrentSectionBar = 0 }
            SongModeDrone -> rt { trCurrentSectionBar = 0 }
        Just choice ->
          rt
            { trCurrentSectionName = tcSectionName choice
            , trCurrentSectionBar = 0
            , trRngState = tcNextSeed choice
            , trTransitionCount = trTransitionCount rt + 1
            , trPendingTransitions = tcTelemetry choice : trPendingTransitions rt
            }

data TransitionChoice = TransitionChoice
  { tcSectionName ∷ !String
  , tcNextSeed    ∷ !Word64
  , tcTelemetry   ∷ !TimelineTransitionTelemetry
  }

pickNextSection ∷ TimelineRuntime → Maybe TransitionChoice
pickNextSection rt =
  let available = M.keys (trSections rt)
      hasEnding = "ending" `elem` available
      forceEnding =
        trMode rt == SongModeCue
          && hasEnding
          && trTransitionCount rt >= 6
          && trCurrentSectionName rt /= "ending"
      cands0 =
        if forceEnding
          then [("ending", 1)]
          else transitionCandidates (trMode rt) (trCurrentSectionName rt) available
      cands = applyRuntimeControlWeights rt cands0
      reason =
        if forceEnding
          then "forced-ending"
          else "phrase-boundary-weighted"
      boundaryBarIx = max 0 (trGeneratedBarCount rt - 1)
      boundaryOffsetFrames = trNextBarOffset rt
  in do
       choice <- weightedPick (trRngState rt) cands
       let telemetry =
             TimelineTransitionTelemetry
               { ttFromSectionName = trCurrentSectionName rt
               , ttToSectionName = wcName choice
               , ttReason = reason
               , ttBoundaryBarIx = boundaryBarIx
               , ttBoundaryOffsetFrames = boundaryOffsetFrames
               , ttBaseWeights = cands0
               , ttFinalWeights = cands
               , ttMoodTarget = trTargetMood rt
               , ttEnergyTarget = trTargetEnergy rt
               , ttPickTicket = wcTicket choice
               , ttPickTotal = wcTotal choice
               }
       pure
         TransitionChoice
           { tcSectionName = wcName choice
           , tcNextSeed = wcNextSeed choice
           , tcTelemetry = telemetry
           }

transitionCandidates ∷ SongMode → String → [String] → [(String, Int)]
transitionCandidates mode current available =
  let scripted =
        case mode of
          SongModeCue ->
            case current of
              "intro" -> [("verse", 10), ("chorus", 2), ("bridge", 1), ("ending", 1)]
              "verse" -> [("chorus", 8), ("verse", 3), ("bridge", 3), ("ending", 2)]
              "chorus" -> [("verse", 5), ("bridge", 3), ("chorus", 2), ("ending", 4)]
              "bridge" -> [("chorus", 7), ("verse", 3), ("ending", 3)]
              "ending" -> []
              _ -> [("verse", 5), ("chorus", 5), ("bridge", 2), ("ending", 2)]
          SongModeDrone ->
            case current of
              "intro" -> [("verse", 6), ("bridge", 4), ("chorus", 2)]
              "verse" -> [("verse", 7), ("bridge", 4), ("chorus", 3), ("ending", 1)]
              "chorus" -> [("chorus", 5), ("verse", 5), ("bridge", 3), ("ending", 1)]
              "bridge" -> [("bridge", 5), ("verse", 4), ("chorus", 4), ("ending", 1)]
              "ending" -> [("verse", 4), ("bridge", 4), ("chorus", 3)]
              _ -> [("verse", 5), ("chorus", 4), ("bridge", 4), ("ending", 1)]
      filtered = filter (\(name, w) -> w > 0 && name `elem` available) scripted
      fallback = filter (/= current) available
  in if null filtered
       then map (\name -> (name, 1)) fallback
       else filtered

applyRuntimeControlWeights ∷ TimelineRuntime → [(String, Int)] → [(String, Int)]
applyRuntimeControlWeights rt cands =
  let adjusted =
        mapMaybe
          (\(name, w) -> scaleCandidateWeight rt name w)
          cands
  in if null adjusted then cands else adjusted

scaleCandidateWeight ∷ TimelineRuntime → String → Int → Maybe (String, Int)
scaleCandidateWeight rt sectionName baseW
  | baseW <= 0 = Nothing
  | otherwise =
      let sectionSpec = lookupSectionSpec rt sectionName
          energyMult = energyBiasWeight (trTargetEnergy rt) sectionName sectionSpec
          moodMult = moodBiasWeight (trTargetMood rt) sectionName sectionSpec
          raw = round (fromIntegral baseW * energyMult * moodMult) ∷ Int
          w' = max 1 raw
      in Just (sectionName, w')

energyBiasWeight ∷ Float → String → Maybe SectionSpec → Float
energyBiasWeight targetEnergy sectionName sectionSpec =
  let profile = sectionEnergyProfile sectionName sectionSpec
      target = clamp01 targetEnergy
      closeness = max 0 (1 - abs (target - profile))
      base = 0.2 + 4.8 * closeness * closeness
      directional
        | target >= 0.7 && profile >= 0.75 = 1.9
        | target >= 0.7 && profile <= 0.3 = 0.45
        | target <= 0.3 && profile <= 0.3 = 1.9
        | target <= 0.3 && profile >= 0.75 = 0.45
        | otherwise = 1
  in base * directional

sectionEnergyProfile ∷ String → Maybe SectionSpec → Float
sectionEnergyProfile sectionName sectionSpec =
  let byName =
        case sectionName of
          "intro" -> 0.20
          "verse" -> 0.55
          "chorus" -> 0.90
          "bridge" -> 0.40
          "ending" -> 0.10
          _ -> 0.50
      hinted =
        case sectionSpec of
          Nothing -> Nothing
          Just spec -> metadataEnergyHint (sectionDescriptorWords spec)
  in case hinted of
       Nothing -> byName
       Just h -> byName * 0.35 + h * 0.65

metadataEnergyHint ∷ [String] → Maybe Float
metadataEnergyHint ws
  | any (`elem` ws) ["aggressive", "intense", "combat", "driving", "heavy", "blast"] = Just 0.90
  | any (`elem` ws) ["tense", "ominous", "suspense", "dark"] = Just 0.65
  | any (`elem` ws) ["calm", "ambient", "drone", "slow", "sparse", "soft"] = Just 0.20
  | any (`elem` ws) ["wide", "floating", "suspended", "dreamy"] = Just 0.40
  | otherwise = Nothing

moodBiasWeight ∷ Maybe String → String → Maybe SectionSpec → Float
moodBiasWeight moodTarget sectionName sectionSpec =
  let legacy =
        case moodTarget of
          Nothing -> 1
          Just mood -> legacyMoodBias mood sectionName
      metadata =
        case (moodTarget, sectionSpec) of
          (Just mood, Just spec) -> moodMetadataBias mood (sectionDescriptorWords spec)
          _ -> 1
  in legacy * 0.35 + metadata * 0.65

legacyMoodBias ∷ String → String → Float
legacyMoodBias mood sectionName =
  case mood of
    "combat" -> combatProfile sectionName
    "aggressive" -> combatProfile sectionName
    "intense" -> combatProfile sectionName
    "calm" -> calmProfile sectionName
    "ambient" -> calmProfile sectionName
    "drone" -> calmProfile sectionName
    "tense" -> tenseProfile sectionName
    "suspense" -> tenseProfile sectionName
    _ -> 1
  where
    combatProfile name =
      case name of
        "chorus" -> 2.3
        "verse" -> 1.25
        "bridge" -> 0.8
        "intro" -> 0.65
        "ending" -> 0.6
        _ -> 1
    calmProfile name =
      case name of
        "bridge" -> 2.0
        "verse" -> 1.4
        "chorus" -> 0.55
        "intro" -> 1.2
        "ending" -> 1.1
        _ -> 1
    tenseProfile name =
      case name of
        "bridge" -> 1.75
        "intro" -> 1.5
        "verse" -> 1.2
        "chorus" -> 0.95
        "ending" -> 0.8
        _ -> 1

moodMetadataBias ∷ String → [String] → Float
moodMetadataBias mood ws =
  let isCombat = any (`elem` ws) ["aggressive", "intense", "combat", "driving", "heavy", "blast"]
      isCalm = any (`elem` ws) ["calm", "ambient", "drone", "slow", "sparse", "soft"]
      isTense = any (`elem` ws) ["tense", "ominous", "suspense", "dark", "suspended"]
  in case mood of
       "combat" -> if isCombat then 2.5 else if isCalm then 0.55 else 1
       "aggressive" -> if isCombat then 2.5 else if isCalm then 0.55 else 1
       "intense" -> if isCombat then 2.2 else if isCalm then 0.6 else 1
       "calm" -> if isCalm then 2.5 else if isCombat then 0.55 else 1
       "ambient" -> if isCalm then 2.5 else if isCombat then 0.55 else 1
       "drone" -> if isCalm then 2.3 else if isCombat then 0.6 else 1
       "tense" -> if isTense then 2.2 else if isCalm then 0.75 else 1
       "suspense" -> if isTense then 2.2 else if isCalm then 0.75 else 1
       _ -> 1

lookupSectionSpec ∷ TimelineRuntime → String → Maybe SectionSpec
lookupSectionSpec rt sectionName = M.lookup sectionName (trSections rt)

sectionDescriptorWords ∷ SectionSpec → [String]
sectionDescriptorWords spec =
  nub (wordsLower (ssName spec) <> wordsLower (ssMood spec) <> wordsLower (ssFeel spec))

wordsLower ∷ String → [String]
wordsLower raw =
  let mapped = map (\c -> if isWordChar c then toLower c else ' ') raw
  in filter (not . null) (words mapped)

isWordChar ∷ Char → Bool
isWordChar c =
  (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')

data WeightedChoice = WeightedChoice
  { wcName     ∷ !String
  , wcNextSeed ∷ !Word64
  , wcTicket   ∷ !Int
  , wcTotal    ∷ !Int
  }

weightedPick ∷ Word64 → [(String, Int)] → Maybe WeightedChoice
weightedPick _ [] = Nothing
weightedPick seed weighted =
  let positive = filter ((> 0) . snd) weighted
      total = sum (map snd positive)
      nextSeed = nextConductorSeed seed
  in if total <= 0
       then Nothing
       else
          let ticket = fromIntegral (nextSeed `mod` fromIntegral total) ∷ Int
          in Just
               WeightedChoice
                 { wcName = pick ticket positive
                 , wcNextSeed = nextSeed
                 , wcTicket = ticket
                 , wcTotal = total
                 }
  where
    pick _ [] = ""
    pick n ((name, w) : xs)
      | n < w = name
      | otherwise = pick (n - w) xs

nextConductorSeed ∷ Word64 → Word64
nextConductorSeed x = x * 6364136223846793005 + 1442695040888963407

varyBarNotes
  ∷ SongMode
  → Float
  → Maybe String
  → SectionSpec
  → Int
  → Word64
  → [TimelineNote]
  → ([TimelineNote], Word64)
varyBarNotes mode targetEnergy targetMood sectionSpec absoluteBarIx seed0 notes0
  | null notes0 = ([], nextConductorSeed seed0)
  | otherwise =
      let instrumentIds = nub (map tnInstrumentId notes0)
          canDropInstrument = length instrumentIds > 1
          mutateOne (groupsAcc, seedAcc) iid =
            let source = filter (\n -> tnInstrumentId n == iid) notes0
                (group', seedN') =
                  mutateInstrumentFromNotes
                    mode
                    targetEnergy
                    targetMood
                    sectionSpec
                    absoluteBarIx
                    canDropInstrument
                    (seedAcc `xor` fromIntegral (instrumentIdInt iid))
                    source
            in (group' : groupsAcc, seedN')
          (groups, seedN) = foldl mutateOne ([], seed0) instrumentIds
          merged = concat (reverse groups)
          fallback =
            case instrumentIds of
              [] -> []
              iid : _ ->
                take 1 (filter (\n -> tnInstrumentId n == iid) notes0)
          out =
            if null merged
              then fallback
              else merged
      in (sortOn (\n -> (tnOnOffsetFrames n, instrumentIdInt (tnInstrumentId n))) out, seedN)

-- Internal helper that applies mutation to one instrument's notes.
mutateInstrumentFromNotes
  ∷ SongMode
  → Float
  → Maybe String
  → SectionSpec
  → Int
  → Bool
  → Word64
  → [TimelineNote]
  → ([TimelineNote], Word64)
mutateInstrumentFromNotes mode targetEnergy targetMood sectionSpec absoluteBarIx canDrop seed0 notes
  | null notes = ([], nextConductorSeed seed0)
  | otherwise =
      let seed1 = nextConductorSeed (seed0 `xor` fromIntegral absoluteBarIx)
          dropRoll = rand01 seed1
          shouldDrop =
            canDrop
              && barDropEligible absoluteBarIx
              && dropRoll < dropoutThreshold mode targetEnergy
      in if shouldDrop
           then ([], nextConductorSeed seed1)
           else
              let seed2 = nextConductorSeed seed1
                  transposeSemis = chooseTranspose mode targetEnergy targetMood sectionSpec absoluteBarIx seed2
                  seed3 = nextConductorSeed seed2
                  density = densityThreshold mode targetEnergy sectionSpec absoluteBarIx seed3
                  (keptRev, seedN) =
                    foldl
                      (selectMutatedNote density transposeSemis)
                      ([], seed3)
                      notes
                  kept = reverse keptRev
                  out =
                    if null kept
                      then
                        case notes of
                          [] -> []
                          firstNote : _ -> [applyTranspose transposeSemis firstNote]
                      else kept
              in (out, seedN)

selectMutatedNote
  ∷ Float
  → Int
  → ([TimelineNote], Word64)
  → TimelineNote
  → ([TimelineNote], Word64)
selectMutatedNote density transposeSemis (acc, seed0) note =
  let seed1 = nextConductorSeed seed0
      keep = rand01 seed1 <= density
  in if keep
       then (applyTranspose transposeSemis note : acc, seed1)
       else (acc, seed1)

barDropEligible ∷ Int → Bool
barDropEligible absoluteBarIx =
  let slot = absoluteBarIx `mod` 8
  in slot == 6 || slot == 7

dropoutThreshold ∷ SongMode → Float → Float
dropoutThreshold mode energy =
  case mode of
    SongModeCue -> clamp01 (0.28 + (0.5 - clamp01 energy) * 0.24)
    SongModeDrone -> clamp01 (0.16 + (0.5 - clamp01 energy) * 0.16)

densityThreshold ∷ SongMode → Float → SectionSpec → Int → Word64 → Float
densityThreshold mode targetEnergy sectionSpec absoluteBarIx seed =
  let base =
        case mode of
          SongModeCue -> 0.86
          SongModeDrone -> 0.94
      cycleNudge =
        case absoluteBarIx `mod` 8 of
          3 -> -0.20
          7 -> -0.28
          _ -> 0
      energyBoost = (clamp01 targetEnergy - 0.5) * 0.35
      feelBoost = sectionFeelDensityBias sectionSpec
      jitter = (rand01 seed - 0.5) * 0.08
      raw = base + cycleNudge + energyBoost + feelBoost + jitter
  in max 0.25 (min 0.99 raw)

chooseTranspose ∷ SongMode → Float → Maybe String → SectionSpec → Int → Word64 → Int
chooseTranspose mode targetEnergy targetMood sectionSpec absoluteBarIx seed =
  let opts =
        case mode of
          SongModeCue -> [0, 0, 2, -2, 3, -3, 5, -5]
          SongModeDrone -> [0, 0, 0, 1, -1, 2, -2]
      ix = fromIntegral (seed `mod` fromIntegral (length opts))
      seeded = opts !! ix
      barBias =
        case absoluteBarIx `mod` 8 of
          1 -> 2
          5 -> -2
          _ -> 0
      energyMag = floor (clamp01 targetEnergy * 2) :: Int
      moodBias =
        case targetMood of
          Just "combat" -> 2
          Just "aggressive" -> 2
          Just "intense" -> 1
          Just "calm" -> -1
          Just "ambient" -> -2
          Just "drone" -> -2
          _ -> 0
      metadataBias = sectionFeelTransposeBias sectionSpec
      total0 = seeded + barBias + energyMag + moodBias + metadataBias
      total =
        case absoluteBarIx `mod` 8 of
          1 -> if total0 == 0 then 2 else total0
          5 -> if total0 == 0 then -2 else total0
          _ -> total0
  in max (-12) (min 12 total)

sectionFeelDensityBias ∷ SectionSpec → Float
sectionFeelDensityBias spec =
  let ws = sectionDescriptorWords spec
  in if any (`elem` ws) ["dense", "busy", "wall", "thick", "driving", "heavy", "aggressive"]
       then 0.18
       else if any (`elem` ws) ["sparse", "minimal", "slow", "spacious", "ambient", "drone", "calm"]
              then -0.18
              else 0

sectionFeelTransposeBias ∷ SectionSpec → Int
sectionFeelTransposeBias spec =
  let ws = sectionDescriptorWords spec
  in if any (`elem` ws) ["aggressive", "intense", "rising", "driving"]
       then 1
       else if any (`elem` ws) ["calm", "ambient", "drone", "slow", "descending"]
              then -1
              else 0

applyTranspose ∷ Int → TimelineNote → TimelineNote
applyTranspose semis note =
  let NoteKey k = tnKey note
      k' = max 0 (min 127 (k + semis))
  in note { tnKey = NoteKey k' }

rand01 ∷ Word64 → Float
rand01 seed =
  let bucket = fromIntegral (seed `mod` 10000) ∷ Float
  in bucket / 10000

instrumentIdInt ∷ InstrumentId → Int
instrumentIdInt iid =
  case iid of
    InstrumentId n -> n

initialSectionName ∷ SongSpec → String
initialSectionName spec =
  let names = M.keys (sgSections spec)
  in if "intro" `elem` names
       then "intro"
       else case names of
         [] -> ""
         x : _ -> x

compileSectionNotes
  ∷ Word32
  → String
  → SectionSpec
  → [InstrumentPatternSpec]
  → [TimelineNote]
compileSectionNotes sampleRateHz sectionName sectionSpec insts =
  concatMap perInstrument insts
  where
    framesPerBeat =
      (fromIntegral sampleRateHz ∷ Double)
        * 60
        / realToFrac (max 1 (ssTempoBpm sectionSpec))
    perInstrument inst =
      let notes = M.findWithDefault [] sectionName (ipPatterns inst)
      in map (compilePatternNote framesPerBeat inst) notes

compilePatternNote ∷ Double → InstrumentPatternSpec → PatternNote → TimelineNote
compilePatternNote framesPerBeat inst note =
  let onFrames = floor (realToFrac (pnBeatOffset note) * framesPerBeat) ∷ Integer
      durFrames = max 1 (floor (realToFrac (pnDuration note) * framesPerBeat) ∷ Integer)
      onOffset = fromIntegral (max 0 onFrames)
      offOffset = fromIntegral (max 1 (onFrames + durFrames))
  in TimelineNote
      { tnInstrumentId = ipInstrumentId inst
      , tnAmp = max 0 (ipAmp inst)
      , tnPan = max (-1) (min 1 (ipPan inst))
      , tnKey = pnNoteKey note
      , tnVelocity = max 0 (min 1 (pnVelocity note))
      , tnOnOffsetFrames = onOffset
      , tnOffOffsetFrames = offOffset
      }

sectionBarFrames ∷ Word32 → SectionSpec → Word64
sectionBarFrames sampleRateHz sectionSpec =
  let beats = max 1 (ssBeatsPerBar sectionSpec)
      tempo = max 1 (ssTempoBpm sectionSpec)
      framesPerBarD =
        (fromIntegral sampleRateHz ∷ Double)
          * 60
          * fromIntegral beats
          / realToFrac tempo
      framesPerBarI = max 1 (floor framesPerBarD ∷ Integer)
  in fromIntegral framesPerBarI

nextBar ∷ TimelineRuntime → Maybe TimelineBar
nextBar rt =
  let i = trNextBarIx rt
      xs = trBars rt
  in if i < 0 || i >= length xs
       then Nothing
       else Just (xs !! i)

parseSections ∷ FlatYaml → Either String (Map String SectionSpec)
parseSections entries = do
  let sectionNames =
        nub
          [ normalizeName name
          | path <- M.keys entries
          , Just ("sections", name) <- [prefix2 path]
          ]
  pairs <- mapM parseSection sectionNames
  pure (M.fromList pairs)
  where
    parseSection name = do
      tempo <- lookupFloatKey ["sections", name, "tempo_bpm"] entries
      beats <- lookupIntKey ["sections", name, "beats_per_bar"] entries
      beatUnit <- lookupIntKeyDefault ["sections", name, "beat_unit"] 4 entries
      barsPerPhrase <- lookupIntKeyDefault ["sections", name, "bars_per_phrase"] 1 entries
      phraseCount <- lookupIntKeyDefault ["sections", name, "phrase_count"] 1 entries
      let mood = lookupTextDefault ["sections", name, "mood"] "" entries
          feel = lookupTextDefault ["sections", name, "feel"] "" entries
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
      amp <- lookupFloatKeyDefault ["instruments", name, "amp"] 1 entries
      pan <- lookupFloatKeyDefault ["instruments", name, "pan"] 0 entries
      ensure (iid >= 0) ("instruments." <> name <> ".instrument_id must be >= 0")
      patterns <- mapM (parseSectionPattern name) sectionNames
      pure
        InstrumentPatternSpec
          { ipName = name
          , ipInstrumentId = InstrumentId iid
          , ipAmp = amp
          , ipPan = pan
          , ipPatterns = M.fromList patterns
          }

    parseSectionPattern instName sectionName = do
      let dottedPath = ["instruments", instName, "patterns", sectionName]
          prefixedPath = ["instruments", instName, "pattern_" <> sectionName]
          raw =
            case M.lookup dottedPath entries of
              Just txt -> txt
              Nothing -> lookupTextDefault prefixedPath "" entries
      notes <- parsePatternNotes sectionName raw
      pure (sectionName, notes)

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
          key <- parseInt "key" keyTxt
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

lookupTextDefault ∷ [String] → String → FlatYaml → String
lookupTextDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> fallback
    Just v -> v

parseInt ∷ String → String → Either String Int
parseInt label raw =
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid " <> label <> ": " <> raw)

parseFloat ∷ String → String → Either String Float
parseFloat label raw =
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid " <> label <> ": " <> raw)

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

normalizeMood ∷ Maybe String → Maybe String
normalizeMood mood =
  case mood of
    Nothing -> Nothing
    Just raw ->
      let m = map toLower (trim raw)
      in if null m then Nothing else Just m

clamp01 ∷ Float → Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

renderPath ∷ [String] → String
renderPath = foldl render ""
  where
    render "" seg = seg
    render acc seg = acc <> "." <> seg

ensure ∷ Bool → String → Either String ()
ensure cond msg =
  if cond then pure () else Left msg
