{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline
  ( SongMode(..)
  , PatternNote(..)
  , FillGeneratorSpec(..)
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
  , setTimelineGenre
  , validateSongGenre
  , timelineNextBarBoundaryFrame
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.Bits (xor)
import Data.List (isInfixOf, nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
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

data FillGeneratorSpec = FillGeneratorSpec
  { fgVariation   ∷ !Float
  , fgDensity     ∷ !Float
  , fgLengthBeats ∷ !Float
  }
  deriving (Eq, Show)

data ChordSpec = ChordSpec
  { csSymbol    ∷ !String
  , csRootClass ∷ !Int
  , csIntervals ∷ ![Int]
  }
  deriving (Eq, Show)

data InstrumentPatternSpec = InstrumentPatternSpec
  { ipName        ∷ !String
  , ipInstrumentId ∷ !InstrumentId
  , ipAmp         ∷ !Float
  , ipPan         ∷ !Float
  , ipVelocityVariation ∷ !Float
  , ipPatterns    ∷ !(Map String [PatternNote])
  , ipFills       ∷ !(Map String [PatternNote])
  , ipFillGenerator ∷ !(Maybe FillGeneratorSpec)
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
  , ssChordBars     ∷ ![ChordSpec]
  , ssMelodyPhrase  ∷ ![PatternNote]
  }
  deriving (Eq, Show)

data SongSpec = SongSpec
  { sgMode          ∷ !SongMode
  , sgGenre         ∷ !String
  , sgMood          ∷ !String
  , sgForm          ∷ ![String]
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
  , trSongGenre          ∷ !String
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
      genre = normalizeName (lookupTextDefault ["song", "genre"] "" entries)
      mood = normalizeName (lookupTextDefault ["song", "mood"] "" entries)
      form = parseNameList (lookupTextDefault ["song", "form"] "" entries)
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
      then arrangeGeneratedInstruments genre sections
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

cueSectionOrder ∷ SongSpec → [String]
cueSectionOrder spec =
  let available = M.keys (sgSections spec)
      has name = name `elem` available
      explicitForm = filter has (map normalizeSectionName (sgForm spec))
      cueTemplate = ["intro", "verse", "chorus", "verse", "bridge", "chorus", "ending"]
      droneTemplate = ["intro", "verse", "bridge", "chorus", "verse", "bridge", "chorus", "ending"]
      template =
        case sgMode spec of
          SongModeCue -> cueTemplate
          SongModeDrone -> droneTemplate
      planned = filter has template
      canonUnique = nub (cueTemplate <> droneTemplate)
      extras = filter (`notElem` canonUnique) available
  in if not (null explicitForm)
       then explicitForm <> filter (`notElem` explicitForm) extras
       else if null planned
              then available
              else planned <> extras

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
              buildBar i =
                let i64 = fromIntegral i ∷ Word64
                    absIx' = absIx + i
                    notes =
                      compileSectionNotes
                        sampleRateHz
                        (sgGenre spec)
                        sectionName
                        sectionSpec
                        (normalizeMood (nonEmptyText (sgMood spec)))
                        0.5
                        Nothing
                        absIx'
                        (i `mod` barsPerPhrase)
                        barsPerPhrase
                        (sgInstruments spec)
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
       , trSongGenre = sgGenre spec
       , trSections = sgSections spec
       , trInstruments = sgInstruments spec
       , trCurrentSectionName = firstSection
      , trCurrentSectionBar = 0
       , trGeneratedBarCount = 0
       , trNextBarOffset = 0
       , trRngState = seed0
        , trTransitionCount = 0
        , trTargetMood = normalizeMood (nonEmptyText (sgMood spec))
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

setTimelineGenre ∷ String → TimelineRuntime → Either String TimelineRuntime
setTimelineGenre genreRaw rt = do
  genre <- validateSongGenre genreRaw
  instruments <-
    if any sectionHasArrangement (M.elems (trSections rt))
      then arrangeGeneratedInstruments genre (trSections rt)
      else pure (trInstruments rt)
  pure
    rt
      { trSongGenre = genre
      , trInstruments = instruments
      }

validateSongGenre ∷ String → Either String String
validateSongGenre genreRaw = do
  let genre = normalizeName genreRaw
  ensure (isSupportedGenre genre) ("unsupported song.genre: " <> genreRaw <> " (expected electronic|ambient|blackmetal|cinematic)")
  pure genre

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

timelineRuntimeEndFrame ∷ TimelineRuntime → Maybe Word64
timelineRuntimeEndFrame rt =
  case reverse (trBars rt) of
    [] -> Nothing
    bar : _ ->
      Just (trStartFrame rt + tbStartOffsetFrames bar + tbLengthFrames bar)

timelineNextBarBoundaryFrame ∷ Word64 → TimelineRuntime → (Maybe Word64, TimelineRuntime)
timelineNextBarBoundaryFrame now rt0 =
  let rt = ensureFutureBarStarts now 1 rt0
  in (firstFutureBoundary now rt, rt)

timelineBoundarySpanFromNextBar ∷ Word64 → Int → TimelineRuntime → (Maybe (Word64, Word64), TimelineRuntime)
timelineBoundarySpanFromNextBar now barsFromNext rt0 =
  let bars' = max 0 barsFromNext
      rt = ensureFutureBarStarts now (bars' + 1) rt0
      futureStarts = futureBarStartFrames now rt
      mStart = firstFutureBoundary now rt
      mEnd =
        if bars' == 0
          then mStart
          else
            case drop bars' futureStarts of
              endFrame : _ -> Just endFrame
              [] ->
                case timelineRuntimeEndFrame rt of
                  Just endFrame
                    | endFrame > now -> Just endFrame
                  _ -> mStart
  in (case (mStart, mEnd) of
        (Just startFrame, Just endFrame) -> Just (startFrame, max startFrame endFrame)
        _ -> Nothing, rt)

popTransitionTelemetry ∷ TimelineRuntime → ([TimelineTransitionTelemetry], TimelineRuntime)
popTransitionTelemetry rt =
  (reverse (trPendingTransitions rt), rt { trPendingTransitions = [] })

firstFutureBoundary ∷ Word64 → TimelineRuntime → Maybe Word64
firstFutureBoundary now rt =
  case futureBarStartFrames now rt of
    frame : _ -> Just frame
    [] ->
      case timelineRuntimeEndFrame rt of
        Just endFrame
          | endFrame > now -> Just endFrame
        _ -> Nothing

futureBarStartFrames ∷ Word64 → TimelineRuntime → [Word64]
futureBarStartFrames now rt =
  [ frame
  | bar <- trBars rt
  , let frame = trStartFrame rt + tbStartOffsetFrames bar
  , frame > now
  ]

ensureFutureBarStarts ∷ Word64 → Int → TimelineRuntime → TimelineRuntime
ensureFutureBarStarts now needed rt
  | needed <= length (futureBarStartFrames now rt) = rt
  | trDone rt = rt
  | otherwise = ensureFutureBarStarts now needed (appendGeneratedBar rt)

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
               (barIx `mod` barsPerPhrase)
               barsPerPhrase
               (trGeneratedBarCount rt)
               (trRngState rt)
                 ( compileSectionNotes
                     (trSampleRateHz rt)
                     (trSongGenre rt)
                     (trCurrentSectionName rt)
                     sectionSpec
                     (trTargetMood rt)
                    (trTargetEnergy rt)
                    (tbNotes <$> lastTimelineBar (trBars rt))
                    (trGeneratedBarCount rt)
                    (barIx `mod` barsPerPhrase)
                    barsPerPhrase
                    (trInstruments rt)
                )
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
  | isEndingSectionName (trCurrentSectionName rt) =
      rt { trDone = True, trCurrentSectionBar = 0 }
  | otherwise =
      case pickNextSection rt of
        Nothing ->
          case trMode rt of
            SongModeCue ->
              if hasEndingSection (M.keys (trSections rt))
                then
                  rt
                    { trCurrentSectionName = preferredEndingSectionName (M.keys (trSections rt))
                    , trCurrentSectionBar = 0
                    , trTransitionCount = trTransitionCount rt + 1
                    }
                else rt { trCurrentSectionBar = 0 }
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
      hasEnding = hasEndingSection available
      forceEnding =
        hasEnding
          &&
          trCurrentSectionName rt /= preferredEndingSectionName available
          &&
          trTransitionCount rt >= forceEndingAfterTransitions (trMode rt)
      cands0 =
        if forceEnding
          then [(preferredEndingSectionName available, 1)]
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

forceEndingAfterTransitions ∷ SongMode → Int
forceEndingAfterTransitions mode =
  case mode of
    SongModeCue -> 6
    SongModeDrone -> 16

transitionCandidates ∷ SongMode → String → [String] → [(String, Int)]
transitionCandidates mode current available =
  let endingName = preferredEndingSectionName available
      scripted =
        if isIntroSectionName current
          then
            [("verse", 100)]
          else
            if isEndingSectionName current
              then []
              else
                if isVerseSectionName current
                  then
                    case mode of
                      SongModeCue -> [("chorus", 7), ("bridge", 3), (endingName, 1)]
                      SongModeDrone -> [("chorus", 5), ("bridge", 4), (endingName, 1)]
                  else
                    if isChorusSectionName current
                      then
                        case mode of
                          SongModeCue -> [("chorus", 8), ("bridge", 3), (endingName, 2)]
                          SongModeDrone -> [("chorus", 9), ("bridge", 4), (endingName, 1)]
                      else
                        if isBridgeSectionName current
                          then
                            case mode of
                              SongModeCue -> [("chorus", 6), ("bridge", 2), (endingName, 2)]
                              SongModeDrone -> [("chorus", 6), ("bridge", 3), (endingName, 1)]
                          else
                            case mode of
                              SongModeCue -> [("verse", 7), ("chorus", 4), ("bridge", 3), (endingName, 2)]
                              SongModeDrone -> [("verse", 6), ("chorus", 5), ("bridge", 4), (endingName, 1)]
      filtered = filter (\(name, w) -> w > 0 && name `elem` available) scripted
      fallback = fallbackCandidates current available
  in if null filtered
       then fallback
       else filtered

fallbackCandidates ∷ String → [String] → [(String, Int)]
fallbackCandidates current available
  | isIntroSectionName current =
      let verse = filter isVerseSectionName available
          body =
            filter
              (\name -> not (isIntroSectionName name) && not (isEndingSectionName name))
              available
          ending = filter isEndingSectionName available
      in map (\name -> (name, 1)) (if null verse then if null body then ending else body else verse)
  | isEndingSectionName current = []
  | otherwise =
      let permitted = filter (\name -> isChorusSectionName name || isBridgeSectionName name || isEndingSectionName name) available
      in if null permitted
           then map (\name -> (name, 1)) (filter (/= current) available)
           else map (\name -> (name, 1)) permitted

hasEndingSection ∷ [String] → Bool
hasEndingSection = any isEndingSectionName

preferredEndingSectionName ∷ [String] → String
preferredEndingSectionName available
  | "ending" `elem` available = "ending"
  | "outro" `elem` available = "outro"
  | otherwise =
      case filter isEndingSectionName available of
        x : _ -> x
        [] ->
          case available of
            x : _ -> x
            [] -> "ending"

isIntroSectionName ∷ String → Bool
isIntroSectionName name = normalizeSectionName name == "intro"

isVerseSectionName ∷ String → Bool
isVerseSectionName name = normalizeSectionName name == "verse"

isChorusSectionName ∷ String → Bool
isChorusSectionName name = normalizeSectionName name == "chorus"

isBridgeSectionName ∷ String → Bool
isBridgeSectionName name = normalizeSectionName name == "bridge"

isEndingSectionName ∷ String → Bool
isEndingSectionName name =
  let n = normalizeSectionName name
  in n == "ending" || n == "outro"

normalizeSectionName ∷ String → String
normalizeSectionName = map toLower . trim

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
  → Int
  → Int
  → Word64
  → [TimelineNote]
  → ([TimelineNote], Word64)
varyBarNotes mode targetEnergy targetMood sectionSpec barInPhrase barsPerPhrase absoluteBarIx seed0 notes0
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
                    barInPhrase
                    barsPerPhrase
                    iid
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
  → Int
  → InstrumentId
  → Int
  → Bool
  → Word64
  → [TimelineNote]
  → ([TimelineNote], Word64)
mutateInstrumentFromNotes mode targetEnergy targetMood sectionSpec barInPhrase barsPerPhrase instrumentId absoluteBarIx canDrop seed0 notes
  | null notes = ([], nextConductorSeed seed0)
  | otherwise =
      let seed1 = nextConductorSeed (seed0 `xor` fromIntegral absoluteBarIx)
          dropRoll = rand01 seed1
          isDrums = isDrumInstrumentId instrumentId
          shouldDrop =
            (not isDrums)
              && canDrop
              && barDropEligible barInPhrase barsPerPhrase absoluteBarIx
              && dropRoll < dropoutThreshold mode targetEnergy sectionSpec
      in if shouldDrop
           then ([], nextConductorSeed seed1)
           else
               let seed2 = nextConductorSeed seed1
                   transposeSemis =
                     if isDrums
                       then 0
                       else chooseTranspose mode targetEnergy targetMood sectionSpec absoluteBarIx seed2
                   seed3 = nextConductorSeed seed2
                   density =
                     if isDrums
                       then 1
                       else densityThreshold mode targetEnergy sectionSpec absoluteBarIx seed3
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

barDropEligible ∷ Int → Int → Int → Bool
barDropEligible barInPhrase barsPerPhrase absoluteBarIx =
  let slot = absoluteBarIx `mod` 8
      phraseEnd = barsPerPhrase > 0 && barInPhrase == (barsPerPhrase - 1)
  in phraseEnd || slot == 6 || slot == 7

dropoutThreshold ∷ SongMode → Float → SectionSpec → Float
dropoutThreshold mode energy sectionSpec =
  let sectionNudge =
        if isChorusSection sectionSpec
          then -0.18
          else if isBridgeSection sectionSpec
                 then 0.12
                 else 0
      base =
        case mode of
          SongModeCue -> 0.28 + (0.5 - clamp01 energy) * 0.24
          SongModeDrone -> 0.16 + (0.5 - clamp01 energy) * 0.16
  in clamp01 (base + sectionNudge)

densityThreshold ∷ SongMode → Float → SectionSpec → Int → Word64 → Float
densityThreshold mode targetEnergy sectionSpec absoluteBarIx seed =
  let base =
        case mode of
          SongModeCue -> 0.86
          SongModeDrone -> 0.94
      cycleNudge0 =
        case absoluteBarIx `mod` 8 of
          3 -> -0.20
          7 -> -0.28
          _ -> 0
      cycleNudge =
        if isChorusSection sectionSpec
          then cycleNudge0 * 0.2
          else if isBridgeSection sectionSpec
                 then cycleNudge0 * 1.25
                 else cycleNudge0
      energyBoost = (clamp01 targetEnergy - 0.5) * 0.35
      feelBoost = sectionFeelDensityBias sectionSpec
      jitterScale =
        if isChorusSection sectionSpec
          then 0.02
          else if isBridgeSection sectionSpec
                 then 0.12
                 else 0.08
      jitter = (rand01 seed - 0.5) * jitterScale
      raw = base + cycleNudge + energyBoost + feelBoost + jitter
  in max 0.25 (min 0.99 raw)

chooseTranspose ∷ SongMode → Float → Maybe String → SectionSpec → Int → Word64 → Int
chooseTranspose mode targetEnergy targetMood sectionSpec absoluteBarIx seed =
  let opts =
        case mode of
          SongModeCue -> [0, 0, 2, -2, 3, -3, 5, -5]
          SongModeDrone -> [0, 0, 0, 1, -1, 2, -2]
      ix = fromIntegral (seed `mod` fromIntegral (length opts))
      seeded0 = opts !! ix
      seeded =
        if isChorusSection sectionSpec
          then
            case seeded0 of
              n | n > 1 -> 1
              n | n < -1 -> -1
              _ -> seeded0
          else
            if isBridgeSection sectionSpec
              then seeded0 + if rand01 (nextConductorSeed seed) > 0.5 then 1 else -1
              else seeded0
      barBias =
        if isChorusSection sectionSpec
          then 0
          else
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
      sectionBias =
        if isChorusSection spec
          then 0.08
          else if isBridgeSection spec
                 then -0.10
                 else 0
      metadataBias =
        if any (`elem` ws) ["dense", "busy", "wall", "thick", "driving", "heavy", "aggressive"]
          then 0.18
          else if any (`elem` ws) ["sparse", "minimal", "slow", "spacious", "ambient", "drone", "calm"]
                 then -0.18
                 else 0
  in sectionBias + metadataBias

sectionFeelTransposeBias ∷ SectionSpec → Int
sectionFeelTransposeBias spec =
  let ws = sectionDescriptorWords spec
      sectionBias =
        if isChorusSection spec
          then 0
          else if isBridgeSection spec
                 then 1
                 else 0
      metadataBias =
        if any (`elem` ws) ["aggressive", "intense", "rising", "driving"]
          then 1
          else if any (`elem` ws) ["calm", "ambient", "drone", "slow", "descending"]
                 then -1
                 else 0
  in sectionBias + metadataBias

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
  → String
  → SectionSpec
  → Maybe String
  → Float
  → Maybe [TimelineNote]
  → Int
  → Int
  → Int
  → [InstrumentPatternSpec]
  → [TimelineNote]
compileSectionNotes sampleRateHz songGenre sectionName sectionSpec targetMood targetEnergy mPrevBarNotes absoluteBarIx barInPhrase barsPerPhrase insts =
  let melodic = concatMap perInstrument insts
      hasDrums =
        any
          (\n -> isDrumInstrumentId (tnInstrumentId n))
          melodic
      fallbackDrums =
        if hasDrums
          then []
          else compileFallbackDrumNotes sectionName sectionSpec absoluteBarIx barInPhrase barsPerPhrase framesPerBeat
  in melodic <> fallbackDrums
  where
    framesPerBeat =
      (fromIntegral sampleRateHz ∷ Double)
        * 60
        / realToFrac (max 1 (ssTempoBpm sectionSpec))
    perInstrument inst =
      let explicitBeatNotes = M.findWithDefault [] sectionName (ipPatterns inst)
          beatNotes =
            if null explicitBeatNotes
              then arrangeInstrumentBar songGenre targetMood sectionSpec absoluteBarIx barInPhrase inst
              else explicitBeatNotes
          fillNotes = M.findWithDefault [] sectionName (ipFills inst)
          useExplicitFill =
            isDrumInstrumentId (ipInstrumentId inst)
              && isPhraseBoundaryBar barInPhrase barsPerPhrase
              && not (null fillNotes)
          generatedFill =
            if useExplicitFill
              then Nothing
              else
                generateFillPattern
                  sectionSpec
                  targetMood
                  targetEnergy
                  (fromMaybe [] mPrevBarNotes)
                  absoluteBarIx
                  barInPhrase
                  barsPerPhrase
                  inst
                  beatNotes
          notes =
            if useExplicitFill
              then fillNotes
              else fromMaybe beatNotes generatedFill
      in map (compilePatternNote framesPerBeat absoluteBarIx (ssBeatsPerBar sectionSpec) inst) notes

arrangeInstrumentBar ∷ String → Maybe String → SectionSpec → Int → Int → InstrumentPatternSpec → [PatternNote]
arrangeInstrumentBar songGenre targetMood sectionSpec absoluteBarIx barInPhrase inst
  | not (sectionHasArrangement sectionSpec) = []
  | otherwise =
      filter (\note -> pnBeatOffset note < beatsPerBar)
        (case normalizeName songGenre of
           "electronic" ->
             case normalizeName (ipName inst) of
               "drums" -> arrangeElectronicDrums targetMood sectionSpec absoluteBarIx barInPhrase
               "bass" -> arrangeElectronicBass targetMood sectionSpec barInPhrase
               "pad" -> arrangeElectronicPad targetMood sectionSpec barInPhrase
               "arp" -> arrangeElectronicArp targetMood sectionSpec absoluteBarIx barInPhrase
               "lead" -> arrangeElectronicLead sectionSpec barInPhrase
               _ -> []
           "ambient" ->
             case normalizeName (ipName inst) of
               "drums" -> arrangeAmbientDrums targetMood sectionSpec absoluteBarIx barInPhrase
               "bass" -> arrangeAmbientBass targetMood sectionSpec barInPhrase
               "pad" -> arrangeAmbientPad targetMood sectionSpec barInPhrase
               "arp" -> arrangeAmbientShimmer targetMood sectionSpec absoluteBarIx barInPhrase
               "lead" -> arrangeAmbientLead sectionSpec barInPhrase
               _ -> []
           "blackmetal" ->
             case normalizeName (ipName inst) of
               "drums" -> arrangeBlackmetalDrums targetMood sectionSpec absoluteBarIx barInPhrase
               "bass" -> arrangeBlackmetalBass targetMood sectionSpec barInPhrase
               "pad" -> arrangeBlackmetalPad targetMood sectionSpec barInPhrase
               "arp" -> arrangeBlackmetalTremolo targetMood sectionSpec absoluteBarIx barInPhrase
               "lead" -> arrangeBlackmetalLead sectionSpec barInPhrase
               _ -> []
           "cinematic" ->
             case normalizeName (ipName inst) of
               "drums" -> arrangeCinematicDrums targetMood sectionSpec absoluteBarIx barInPhrase
               "bass" -> arrangeCinematicBass targetMood sectionSpec barInPhrase
               "pad" -> arrangeCinematicPad targetMood sectionSpec barInPhrase
               "arp" -> arrangeCinematicOstinato targetMood sectionSpec absoluteBarIx barInPhrase
               "lead" -> arrangeCinematicLead sectionSpec barInPhrase
               _ -> []
           _ -> [])
  where
    beatsPerBar = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float

sectionHasArrangement ∷ SectionSpec → Bool
sectionHasArrangement spec = not (null (ssChordBars spec)) || not (null (ssMelodyPhrase spec))

arrangeElectronicLead ∷ SectionSpec → Int → [PatternNote]
arrangeElectronicLead sectionSpec barInPhrase =
  let localNotes = melodyNotesForBar sectionSpec barInPhrase
  in if null localNotes
       then
         [ mkPatternNote beat key dur (vel * 0.78)
         | (beat, key, dur, vel) <- harmonicSparklePattern 5 0.5 0.75 0.36 sectionSpec barInPhrase
         ]
       else localNotes

arrangeCinematicLead ∷ SectionSpec → Int → [PatternNote]
arrangeCinematicLead sectionSpec barInPhrase =
  let localNotes = melodyNotesForBar sectionSpec barInPhrase
      chord = chordForBar sectionSpec barInPhrase
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      topVoice =
        case reverse (chordVoicing 5 chord) of
          key : _ -> key
          [] -> midiFromPitchClass 5 0
  in if null localNotes
       then [mkPatternNote 0 topVoice (max 1 (beats - 0.25)) 0.48]
       else
         [ note
             { pnDuration = max (pnDuration note) 0.5
             , pnVelocity = clamp01 (pnVelocity note * 0.92)
             }
         | note <- localNotes
         ]

arrangeElectronicPad ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeElectronicPad targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      vel
        | isIntroSectionName (ssName sectionSpec) = 0.34
        | hasAny descWords ["dramatic", "tense", "combat"] = 0.46
        | otherwise = 0.38
      lengthBeats = max 1 (fromIntegral (ssBeatsPerBar sectionSpec) - if hasAny descWords ["staccato", "pulse"] then 0.5 else 0.12)
      voicing = harmonicVoicingForBar 4 sectionSpec barInPhrase
  in [ mkPatternNote 0 key lengthBeats vel | key <- voicing ]

arrangeCinematicPad ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeCinematicPad targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      vel
        | hasAny descWords ["dramatic", "intense", "combat"] = 0.54
        | hasAny descWords ["ambient", "sparse", "calm"] = 0.36
        | otherwise = 0.44
      lengthBeats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec))
      voicing = harmonicVoicingForBar 4 sectionSpec barInPhrase
  in [ mkPatternNote 0 key (max 1 (lengthBeats - 0.05)) vel | key <- voicing ]

arrangeElectronicBass ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeElectronicBass targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      root = bassRootMidi 2 sectionSpec barInPhrase
      support = bassSupportMidi 2 sectionSpec barInPhrase
      upper = bassUpperMidi 3 sectionSpec barInPhrase
      approach = bassApproachMidi 2 sectionSpec barInPhrase
      introPattern = [mkPatternNote 0 root beats 0.74]
      sparsePattern =
        [ mkPatternNote 0 root (max 1 (beats / 2 - 0.1)) 0.82
        , mkPatternNote (max 0 (beats / 2)) support 0.6 0.70
        , mkPatternNote (max 0 (beats - 0.75)) approach 0.55 0.68
        ]
      versePattern =
        [ mkPatternNote 0 root 0.75 0.84
        , mkPatternNote 1.5 support 0.5 0.74
        , mkPatternNote 2.5 root 0.5 0.72
        , mkPatternNote (max 0 3.25) approach 0.45 0.70
        ]
      chorusPattern =
        [ mkPatternNote 0 root 0.5 0.88
        , mkPatternNote 1 support 0.5 0.76
        , mkPatternNote 2 upper 0.5 0.82
        , mkPatternNote 3 approach 0.45 0.84
        ]
      bridgePattern =
        [ mkPatternNote 0 root 0.5 0.82
        , mkPatternNote 1.0 support 0.5 0.74
        , mkPatternNote 2.0 upper 0.5 0.78
        , mkPatternNote 3.0 approach 0.45 0.72
        ]
  in if isIntroSectionName (ssName sectionSpec)
       then introPattern
       else if isEndingSectionName (ssName sectionSpec) || hasAny descWords ["ambient", "minimal", "sparse"]
              then sparsePattern
              else if isChorusSectionName (ssName sectionSpec)
                     then chorusPattern
                     else if isBridgeSectionName (ssName sectionSpec)
                     then bridgePattern
                             else versePattern

arrangeCinematicBass ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeCinematicBass targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      root = bassRootMidi 2 sectionSpec barInPhrase
      support = bassSupportMidi 2 sectionSpec barInPhrase
      approach = bassApproachMidi 2 sectionSpec barInPhrase
      upper = bassUpperMidi 3 sectionSpec barInPhrase
      sustained =
        [ mkPatternNote 0 root (max 1 (beats / 2 - 0.1)) 0.70
        , mkPatternNote (max 0 (beats / 2)) support 0.75 0.56
        , mkPatternNote (max 0 (beats - 0.75)) approach 0.60 0.52
        ]
      pulse =
        [ mkPatternNote 0 root 0.9 0.78
        , mkPatternNote 1.5 support 0.6 0.66
        , mkPatternNote 2.5 upper 0.6 0.62
        , mkPatternNote 3.25 approach 0.45 0.60
        ]
  in if isIntroSectionName (ssName sectionSpec) || isEndingSectionName (ssName sectionSpec) || hasAny descWords ["ambient", "calm", "sparse"]
       then sustained
       else pulse

arrangeElectronicArp ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeElectronicArp targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      step
        | hasAny descWords ["dramatic", "tense", "frantic", "dense"] = 0.5
        | otherwise = 1.0
      sustain = min 0.75 step
      baseVel
        | hasAny descWords ["intro", "ambient", "outro", "ending"] = 0.28
        | otherwise = 0.36
  in if isIntroSectionName (ssName sectionSpec)
       then []
       else
         [ mkPatternNote beat key sustain vel
         | (beat, key, _dur, vel) <- harmonicSparklePattern 5 step sustain baseVel sectionSpec (absoluteBarIx + barInPhrase)
         ]

arrangeCinematicOstinato ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeCinematicOstinato targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      step
        | hasAny descWords ["dramatic", "intense", "combat", "driving"] = 0.5
        | otherwise = 1.0
      sustain = min 0.8 step
      baseVel
        | hasAny descWords ["ambient", "calm", "sparse", "intro", "ending"] = 0.22
        | otherwise = 0.34
  in if isIntroSectionName (ssName sectionSpec) && not (hasAny descWords ["dramatic", "intense"])
       then []
       else
         [ mkPatternNote beat key sustain vel
         | (beat, key, _dur, vel) <- harmonicSparklePattern 5 step sustain baseVel sectionSpec (absoluteBarIx + barInPhrase * 2)
         ]

arrangeAmbientLead ∷ SectionSpec → Int → [PatternNote]
arrangeAmbientLead sectionSpec barInPhrase =
  let localNotes = melodyNotesForBar sectionSpec barInPhrase
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      topVoice =
        case reverse (harmonicVoicingForBar 5 sectionSpec barInPhrase) of
          key : _ -> key
          [] -> midiFromPitchClass 5 0
  in if null localNotes
       then [mkPatternNote 0 topVoice (max 1 (beats - 0.25)) 0.26]
       else [ note { pnDuration = max 0.9 (pnDuration note), pnVelocity = clamp01 (pnVelocity note * 0.72) } | note <- localNotes ]

arrangeAmbientPad ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeAmbientPad targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      voicing = harmonicVoicingForBar 4 sectionSpec barInPhrase
      vel
        | hasAny descWords ["dark", "tense", "dramatic"] = 0.34
        | otherwise = 0.28
  in [ mkPatternNote 0 key (max 1 (beats - 0.05)) vel | key <- voicing ]

arrangeAmbientBass ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeAmbientBass targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      root = bassRootMidi 2 sectionSpec barInPhrase
      support = bassSupportMidi 2 sectionSpec barInPhrase
      approach = bassApproachMidi 2 sectionSpec barInPhrase
  in if hasAny descWords ["dramatic", "tense"]
       then
         [ mkPatternNote 0 root (max 1 (beats - 0.8)) 0.58
         , mkPatternNote (max 0 (beats - 0.75)) approach 0.60 0.42
         ]
       else
         [ mkPatternNote 0 root (max 1 (beats - 0.2)) 0.48
         , mkPatternNote (max 0 (beats / 2)) support 0.8 0.34
         ]

arrangeAmbientShimmer ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeAmbientShimmer targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      step
        | hasAny descWords ["dramatic", "tense", "rising"] = 1.0
        | otherwise = 2.0
      sustain = max 0.6 (min 1.8 step)
      baseVel
        | hasAny descWords ["dramatic", "tense"] = 0.24
        | otherwise = 0.18
  in [ mkPatternNote beat key sustain vel
     | (beat, key, _dur, vel) <- harmonicSparklePattern 6 step sustain baseVel sectionSpec (absoluteBarIx + barInPhrase)
     ]

arrangeBlackmetalLead ∷ SectionSpec → Int → [PatternNote]
arrangeBlackmetalLead sectionSpec barInPhrase =
  let localNotes = melodyNotesForBar sectionSpec barInPhrase
  in if null localNotes
       then [ mkPatternNote beat key 0.22 vel | (beat, key, _dur, vel) <- harmonicSparklePattern 5 0.25 0.22 0.46 sectionSpec barInPhrase ]
       else [ note { pnDuration = max 0.2 (min 0.5 (pnDuration note)), pnVelocity = clamp01 (pnVelocity note * 0.92) } | note <- localNotes ]

arrangeBlackmetalPad ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeBlackmetalPad targetMood sectionSpec barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      voicing = harmonicVoicingForBar 4 sectionSpec barInPhrase
      vel
        | hasAny descWords ["ambient", "sparse"] = 0.30
        | otherwise = 0.42
  in [ mkPatternNote 0 key (max 1 (beats - 0.08)) vel | key <- voicing ]

arrangeBlackmetalBass ∷ Maybe String → SectionSpec → Int → [PatternNote]
arrangeBlackmetalBass _targetMood sectionSpec barInPhrase =
  let beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      root = bassRootMidi 2 sectionSpec barInPhrase
      support = bassSupportMidi 2 sectionSpec barInPhrase
      upper = bassUpperMidi 3 sectionSpec barInPhrase
      approach = bassApproachMidi 2 sectionSpec barInPhrase
      slots = takeWhile (< beats) [0, 0.5 ..]
      keyAt ix
        | ix == length slots - 1 = approach
        | ix `mod` 4 == 0 = root
        | ix `mod` 4 == 1 = support
        | ix `mod` 4 == 2 = root
        | otherwise = upper
  in [ mkPatternNote beat (keyAt ix) 0.28 (0.64 + fromIntegral (ix `mod` 2) * 0.06)
     | (ix, beat) <- zip [0 :: Int ..] slots
     ]

arrangeBlackmetalTremolo ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeBlackmetalTremolo _targetMood sectionSpec absoluteBarIx barInPhrase =
  [ mkPatternNote beat key 0.18 vel
  | (beat, key, _dur, vel) <- harmonicSparklePattern 5 0.25 0.18 0.46 sectionSpec (absoluteBarIx * 2 + barInPhrase)
  ]

arrangeElectronicDrums ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeElectronicDrums targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
      barProgress =
        if barsPerPhrase <= 1
          then 1
          else fromIntegral barInPhrase / fromIntegral (barsPerPhrase - 1)
      kick beat vel = mkPatternNote beat 36 0.25 vel
      snare beat vel = mkPatternNote beat 38 0.24 vel
      clap beat vel = mkPatternNote beat 39 0.18 vel
      hat key beat dur vel = mkPatternNote beat key dur vel
      crash beat dur vel = mkPatternNote beat 49 dur vel
      ride beat dur vel = mkPatternNote beat 51 dur vel
      introNotes =
        let swirlStep
              | barProgress > 0.85 = 0.25
              | barProgress > 0.45 = 0.5
              | otherwise = 1.0
            swirlStarts = beatGrid swirlStep beats
            swirl =
              [ hat
                  (if ix `mod` 3 == 0 then 46 else 49)
                  beat
                  (0.22 + swirlStep * 0.30)
                  (0.26 + barProgress * 0.28 + fromIntegral (ix `mod` 4) * 0.04)
              | (ix, beat) <- zip [0 :: Int ..] swirlStarts
              ]
            dramaticHits =
              [ snare 2.5 (0.68 + barProgress * 0.10)
              , clap 2.5 (0.36 + barProgress * 0.08)
              , snare (max 0 (beats - 0.5)) (0.80 + barProgress * 0.12)
              , crash 0 0.75 0.42
              ]
        in filter (\note -> pnBeatOffset note < beats) (swirl <> dramaticHits)
      verseNotes =
        [ kick 0 0.92
        , kick 2.5 0.80
        , snare 1 0.84
        , snare 3 0.88
        ]
          <> [ hat 42 beat 0.16 (0.42 + if beat == 0 || beat == 2 then 0.04 else 0)
             | beat <- beatGrid 0.5 beats
             ]
          <> [ hat 46 3.5 0.28 0.52 | beats >= 4 ]
      chorusNotes =
        [ kick 0 0.96
        , kick 1.5 0.78
        , kick 2 0.90
        , kick 3.25 0.82
        , snare 1 0.88
        , snare 3 0.92
        , clap 1 0.24
        , clap 3 0.28
        ]
          <> [ hat (if ix `mod` 4 == 3 then 46 else 42) beat 0.18 (0.46 + fromIntegral (ix `mod` 2) * 0.05)
             | (ix, beat) <- zip [0 :: Int ..] (beatGrid 0.5 beats)
             ]
          <> [ crash 0 0.65 0.54 ]
      bridgeNotes =
        [ kick 0 0.88
        , kick 2 0.76
        , snare 1 0.78
        , snare 3 0.84
        ]
          <> [ ride beat 0.20 (0.44 + fromIntegral (ix `mod` 3) * 0.04)
             | (ix, beat) <- zip [0 :: Int ..] (beatGrid 1.0 beats)
             ]
          <> [ hat key beat 0.18 0.48
             | (key, beat) <- zip [45, 47, 50, 47] [2.0, 2.5, 3.0, 3.5]
             , beat < beats
             ]
      endingNotes =
        [ kick 0 0.86
        , snare (beats - 1) 0.74
        , crash 0 0.80 0.40
        ]
          <> [ hat 46 (beats - 0.5) 0.24 0.36 | beats >= 2 ]
      baseNotes
        | isIntroSectionName (ssName sectionSpec) = introNotes
        | isEndingSectionName (ssName sectionSpec) = endingNotes
        | isBridgeSectionName (ssName sectionSpec) = bridgeNotes
        | isChorusSectionName (ssName sectionSpec) || hasAny descWords ["dramatic", "intense", "combat"] = chorusNotes
        | otherwise = verseNotes
  in map (humanizeElectronicDrum absoluteBarIx) (filter (\note -> pnBeatOffset note < beats) baseNotes)

arrangeCinematicDrums ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeCinematicDrums targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
      barProgress =
        if barsPerPhrase <= 1
          then 1
          else fromIntegral barInPhrase / fromIntegral (barsPerPhrase - 1)
      kick beat vel = mkPatternNote beat 36 0.3 vel
      snare beat vel = mkPatternNote beat 38 0.24 vel
      tom key beat vel = mkPatternNote beat key 0.22 vel
      cym key beat dur vel = mkPatternNote beat key dur vel
      introNotes =
        [ cym 49 0 0.9 0.38
        , cym 49 (max 0 (beats - 1)) 0.8 (0.34 + barProgress * 0.14)
        , snare (max 0 (beats - 0.5)) (0.70 + barProgress * 0.12)
        ]
          <> [ cym 46 beat 0.35 (0.22 + fromIntegral ix * 0.04)
             | (ix, beat) <- zip [0 :: Int ..] (beatGrid 1.0 beats)
             ]
      verseNotes =
        [ kick 0 0.82
        , snare 2 0.76
        ]
          <> [ cym 51 beat 0.35 (0.32 + fromIntegral (ix `mod` 2) * 0.04)
             | (ix, beat) <- zip [0 :: Int ..] (beatGrid 1.0 beats)
             ]
      chorusNotes =
        [ kick 0 0.90
        , kick 2 0.82
        , snare 1 0.84
        , snare 3 0.90
        , tom 45 3.5 0.62
        ]
          <> [ cym 49 0 0.8 0.46
             , cym 51 1.5 0.4 0.34
             , cym 51 2.5 0.4 0.34
             ]
      bridgeNotes =
        [ kick 0 0.86
        , tom 43 2.0 0.60
        , tom 47 2.5 0.56
        , snare 3 0.82
        ]
          <> [ cym 51 beat 0.35 (0.30 + fromIntegral ix * 0.03)
             | (ix, beat) <- zip [0 :: Int ..] [0.0, 1.0, 2.0, 3.0]
             , beat < beats
             ]
      endingNotes =
        [ kick 0 0.72
        , tom 43 (max 0 (beats - 1)) 0.50
        , cym 49 0 1.0 0.34
        ]
      baseNotes
        | isIntroSectionName (ssName sectionSpec) = introNotes
        | isEndingSectionName (ssName sectionSpec) = endingNotes
        | isBridgeSectionName (ssName sectionSpec) = bridgeNotes
        | isChorusSectionName (ssName sectionSpec) || hasAny descWords ["dramatic", "intense", "combat"] = chorusNotes
         | otherwise = verseNotes
  in map (humanizeElectronicDrum absoluteBarIx) (filter (\note -> pnBeatOffset note < beats) baseNotes)

arrangeAmbientDrums ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeAmbientDrums targetMood sectionSpec absoluteBarIx barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
      barProgress =
        if barsPerPhrase <= 1
          then 1
          else fromIntegral barInPhrase / fromIntegral (barsPerPhrase - 1)
      cym key beat dur vel = mkPatternNote beat key dur vel
      tom key beat vel = mkPatternNote beat key 0.28 vel
      introNotes =
        [ cym 49 0 1.2 0.26
        , cym 49 (max 0 (beats - 0.75)) 1.0 (0.24 + barProgress * 0.10)
        ]
          <> [ cym 46 beat 0.6 (0.16 + fromIntegral ix * 0.03)
             | (ix, beat) <- zip [0 :: Int ..] (beatGrid 2.0 beats)
             ]
      verseNotes =
        [ cym 51 beat 0.55 (0.20 + fromIntegral (ix `mod` 2) * 0.03)
        | (ix, beat) <- zip [0 :: Int ..] (beatGrid 2.0 beats)
        ]
          <> [ tom 43 (beats - 1) 0.28 | hasAny descWords ["dramatic", "tense", "dark"] ]
      chorusNotes =
        [ cym 49 0 1.1 0.32
        , cym 51 2.0 0.7 0.24
        , tom 45 (max 0 (beats - 0.5)) 0.40
        ]
      endingNotes =
        [ cym 49 0 1.4 0.24
        , cym 46 (max 0 (beats - 1)) 0.8 0.18
        ]
      baseNotes
        | isIntroSectionName (ssName sectionSpec) = introNotes
        | isEndingSectionName (ssName sectionSpec) = endingNotes
        | isChorusSectionName (ssName sectionSpec) || hasAny descWords ["dramatic", "tense", "combat"] = chorusNotes
        | otherwise = verseNotes
  in map (humanizeElectronicDrum absoluteBarIx) (filter (\note -> pnBeatOffset note < beats) baseNotes)

arrangeBlackmetalDrums ∷ Maybe String → SectionSpec → Int → Int → [PatternNote]
arrangeBlackmetalDrums targetMood sectionSpec absoluteBarIx _barInPhrase =
  let descWords = arrangementDescriptorWords targetMood sectionSpec
      beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      blastSlots = beatGrid 0.5 beats
      tremSlots = beatGrid 0.25 beats
      kick beat vel = mkPatternNote beat 36 0.22 vel
      snare beat vel = mkPatternNote beat 38 0.18 vel
      hat key beat dur vel = mkPatternNote beat key dur vel
      crash beat dur vel = mkPatternNote beat 49 dur vel
      tom key beat vel = mkPatternNote beat key 0.20 vel
      introNotes =
        [ crash 0 0.9 0.42
        , snare (max 0 (beats - 0.5)) 0.74
        ]
          <> [ hat 46 beat 0.22 (0.22 + fromIntegral ix * 0.03)
             | (ix, beat) <- zip [0 :: Int ..] blastSlots
             ]
      verseNotes =
        [ kick beat (0.78 + fromIntegral (ix `mod` 2) * 0.06)
        | (ix, beat) <- zip [0 :: Int ..] blastSlots
        ]
          <> [ snare beat (0.74 + fromIntegral (ix `mod` 2) * 0.05)
             | (ix, beat) <- zip [0 :: Int ..] [0.5, 1.5 ..]
             , beat < beats
             ]
          <> [ hat 51 beat 0.16 (0.38 + fromIntegral (ix `mod` 3) * 0.03)
             | (ix, beat) <- zip [0 :: Int ..] tremSlots
             ]
      chorusNotes =
        [ kick beat (0.84 + fromIntegral (ix `mod` 2) * 0.06)
        | (ix, beat) <- zip [0 :: Int ..] blastSlots
        ]
          <> [ snare beat (0.82 + fromIntegral (ix `mod` 2) * 0.04)
             | (ix, beat) <- zip [0 :: Int ..] [0.5, 1.5 ..]
             , beat < beats
             ]
          <> [ hat 49 0 0.8 0.46
             ]
          <> [ hat 46 beat 0.14 (0.42 + fromIntegral (ix `mod` 4) * 0.03)
             | (ix, beat) <- zip [0 :: Int ..] tremSlots
             ]
      bridgeNotes =
        [ kick beat (0.76 + fromIntegral (ix `mod` 2) * 0.05)
        | (ix, beat) <- zip [0 :: Int ..] blastSlots
        ]
          <> [ snare 1.5 0.76
             , snare 3.0 0.82
             , tom 45 2.5 0.58
             , tom 47 3.0 0.54
             ]
          <> [ hat 51 beat 0.16 0.34
             | beat <- beatGrid 0.5 beats
             ]
      endingNotes =
        [ crash 0 1.0 0.40
        , kick 0 0.72
        , tom 43 (max 0 (beats - 0.5)) 0.50
        ]
      baseNotes
        | isIntroSectionName (ssName sectionSpec) = introNotes
        | isEndingSectionName (ssName sectionSpec) = endingNotes
        | isBridgeSectionName (ssName sectionSpec) = bridgeNotes
        | isChorusSectionName (ssName sectionSpec) || hasAny descWords ["intense", "aggressive", "combat", "dramatic"] = chorusNotes
        | otherwise = verseNotes
  in map (humanizeElectronicDrum absoluteBarIx) (filter (\note -> pnBeatOffset note < beats) baseNotes)

humanizeElectronicDrum ∷ Int → PatternNote → PatternNote
humanizeElectronicDrum absoluteBarIx note =
  let key = patternNoteKeyInt note
      seed = fromIntegral (absoluteBarIx * 131 + floor (pnBeatOffset note * 37) + key * 17)
      timeJitter
        | key == 49 || key == 46 = (randSigned seed) * 0.015
        | key == 42 = (randSigned seed) * 0.008
        | otherwise = (randSigned seed) * 0.004
      velocityJitter = (randSigned (seed + 97)) * if key == 49 || key == 46 then 0.10 else 0.06
  in note
       { pnBeatOffset = max 0 (pnBeatOffset note + timeJitter)
       , pnVelocity = clamp01 (pnVelocity note + velocityJitter)
       }

arrangementDescriptorWords ∷ Maybe String → SectionSpec → [String]
arrangementDescriptorWords targetMood sectionSpec =
  nub (sectionDescriptorWords sectionSpec <> maybe [] wordsLower targetMood)

chordForBar ∷ SectionSpec → Int → ChordSpec
chordForBar sectionSpec barInPhrase =
  case ssChordBars sectionSpec of
    [] -> ChordSpec "C" 0 [0, 4, 7]
    chords -> chords !! (barInPhrase `mod` length chords)

nextChordForBar ∷ SectionSpec → Int → ChordSpec
nextChordForBar sectionSpec barInPhrase = chordForBar sectionSpec (barInPhrase + 1)

melodyNotesForBar ∷ SectionSpec → Int → [PatternNote]
melodyNotesForBar sectionSpec barInPhrase =
  let beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      startBeat = fromIntegral barInPhrase * beats
      endBeat = startBeat + beats
  in [ note { pnBeatOffset = pnBeatOffset note - startBeat }
     | note <- ssMelodyPhrase sectionSpec
     , pnBeatOffset note >= startBeat
     , pnBeatOffset note < endBeat
     ]

melodyAnchorForBar ∷ SectionSpec → Int → Maybe PatternNote
melodyAnchorForBar sectionSpec barInPhrase =
  case sortOn anchorRank (melodyNotesForBar sectionSpec barInPhrase) of
    note : _ -> Just note
    [] -> Nothing
  where
    beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
    isStrongBeat beat =
      let roundedBeat = fromIntegral (round beat ∷ Int) ∷ Float
      in abs (beat - roundedBeat) <= 0.08
    anchorRank note =
      let noteEnd = min beats (pnBeatOffset note + pnDuration note)
          strongBeatPenalty = if isStrongBeat (pnBeatOffset note) then 0 ∷ Int else 1
          durationRank = negate (round (pnDuration note * 100) ∷ Int)
          velocityRank = negate (round (pnVelocity note * 100) ∷ Int)
          endRank = negate (round (noteEnd * 100) ∷ Int)
      in (strongBeatPenalty, durationRank, velocityRank, endRank)

melodyAnchorPitchClassForBar ∷ SectionSpec → Int → Maybe Int
melodyAnchorPitchClassForBar sectionSpec barInPhrase =
  (`mod` 12) . patternNoteKeyInt <$> melodyAnchorForBar sectionSpec barInPhrase

harmonicAccentPitchClassForBar ∷ SectionSpec → Int → ChordSpec → Int
harmonicAccentPitchClassForBar sectionSpec barInPhrase chord =
  fromMaybe
    (chordColorPitchClass chord)
    (melodyAnchorPitchClassForBar sectionSpec barInPhrase)

chordVoicing ∷ Int → ChordSpec → [Int]
chordVoicing octave chord =
  let root = midiFromPitchClass octave (csRootClass chord)
  in root : [ root + interval | interval <- drop 1 (csIntervals chord) ]

chordPitchClasses ∷ ChordSpec → [Int]
chordPitchClasses chord =
  nub [ (csRootClass chord + (interval `mod` 12)) `mod` 12 | interval <- csIntervals chord ]

chordThirdPitchClass ∷ ChordSpec → Int
chordThirdPitchClass chord =
  let pcs = chordPitchClasses chord
      rootPc = csRootClass chord
      preferred =
        filter (`elem` pcs)
          [ (rootPc + 3) `mod` 12
          , (rootPc + 4) `mod` 12
          , (rootPc + 5) `mod` 12
          , (rootPc + 2) `mod` 12
          ]
  in case preferred of
       pc : _ -> pc
       [] -> rootPc

chordFifthPitchClass ∷ ChordSpec → Int
chordFifthPitchClass chord =
  let pcs = chordPitchClasses chord
      rootPc = csRootClass chord
      preferred =
        filter (`elem` pcs)
          [ (rootPc + 7) `mod` 12
          , (rootPc + 6) `mod` 12
          , (rootPc + 8) `mod` 12
          ]
  in case preferred of
       pc : _ -> pc
       [] -> chordThirdPitchClass chord

chordColorPitchClass ∷ ChordSpec → Int
chordColorPitchClass chord =
  let pcs = chordPitchClasses chord
      rootPc = csRootClass chord
      preferred =
        filter (`elem` pcs)
          [ (rootPc + 10) `mod` 12
          , (rootPc + 11) `mod` 12
          , (rootPc + 2) `mod` 12
          , (rootPc + 9) `mod` 12
          ]
  in case preferred of
       pc : _ -> pc
       [] -> chordThirdPitchClass chord

padPitchClasses ∷ ChordSpec → [Int]
padPitchClasses chord =
  let rootPc = csRootClass chord
      thirdPc = chordThirdPitchClass chord
      fifthPc = chordFifthPitchClass chord
      colorPc = chordColorPitchClass chord
      pcs = nub [rootPc, thirdPc, fifthPc, colorPc]
  in case pcs of
       [a, b, c] -> [a, b, c, a]
       [a, b] -> [a, b, a, b]
       [a] -> [a, a, a, a]
       xs -> take 4 xs

padPitchClassesForBar ∷ SectionSpec → Int → ChordSpec → [Int]
padPitchClassesForBar sectionSpec barInPhrase chord =
  case melodyAnchorPitchClassForBar sectionSpec barInPhrase of
    Nothing -> padPitchClasses chord
    Just anchorPc ->
      let rootPc = csRootClass chord
          supportPcs =
            nub
              ( rootPc
                  : filter
                      (\pc -> pc /= rootPc && pc /= anchorPc)
                      [ chordThirdPitchClass chord
                      , chordFifthPitchClass chord
                      , chordColorPitchClass chord
                      ]
              )
          body = take 3 supportPcs
          fillPc =
            case reverse body of
              pc : _ -> pc
              [] -> rootPc
      in take 4 (body <> [anchorPc] <> repeat fillPc)

pitchClassMidiAtOrAbove ∷ Int → Int → Int
pitchClassMidiAtOrAbove lower pitchClass =
  case [n | n <- [lower .. lower + 24], n `mod` 12 == pitchClass `mod` 12] of
    n : _ -> n
    [] -> lower

basePitchClassVoicing ∷ Int → [Int] → [Int]
basePitchClassVoicing octave pitchClasses =
  case pitchClasses of
    pc0 : pc1 : pc2 : pc3 : _ ->
      let root0 = pitchClassMidiAtOrAbove (midiFromPitchClass octave pc0) pc0
          note1 = pitchClassMidiAtOrAbove (root0 + 3) pc1
          note2 = pitchClassMidiAtOrAbove (note1 + 3) pc2
          note3 = pitchClassMidiAtOrAbove (note2 + 2) pc3
      in [root0, note1, note2, note3]
    _ ->
      let root0 = midiFromPitchClass octave 0
      in [root0, root0 + 4, root0 + 7, root0 + 12]

baseChordVoicing ∷ Int → ChordSpec → [Int]
baseChordVoicing octave chord = basePitchClassVoicing octave (padPitchClasses chord)

rotateVoicingUp ∷ [Int] → [Int]
rotateVoicingUp [] = []
rotateVoicingUp (x : xs) = xs <> [x + 12]

candidateChordVoicingsForBar ∷ Int → SectionSpec → Int → [[Int]]
candidateChordVoicingsForBar octave sectionSpec barInPhrase =
  let chord = chordForBar sectionSpec barInPhrase
      baseVoicing = basePitchClassVoicing octave (padPitchClassesForBar sectionSpec barInPhrase chord)
  in take 4 (iterate rotateVoicingUp baseVoicing)

voicingDistance ∷ [Int] → [Int] → Int
voicingDistance left right = sum (zipWith (\a b -> abs (a - b)) left right)

harmonicVoicingForBar ∷ Int → SectionSpec → Int → [Int]
harmonicVoicingForBar octave sectionSpec barInPhrase =
  let candidatesFor ix = candidateChordVoicingsForBar octave sectionSpec ix
      choose ix prevVoicing candidates =
        case sortOn (scoreFor ix prevVoicing) candidates of
          voicing : _ -> voicing
          [] -> baseChordVoicing octave (chordForBar sectionSpec barInPhrase)
        where
          scoreFor barIx previousVoicing voicing =
            let voiceLeadingScore =
                  case previousVoicing of
                    [] -> sum (map (\n -> abs (n - midiFromPitchClass octave 0)) voicing)
                    _ -> voicingDistance previousVoicing voicing
                melodyScore =
                  case melodyAnchorForBar sectionSpec barIx of
                    Nothing -> 0
                    Just note ->
                      let anchorKey = patternNoteKeyInt note
                          anchorPc = anchorKey `mod` 12
                          topVoice =
                            case reverse voicing of
                              key : _ -> key
                              [] -> midiFromPitchClass octave (csRootClass (chordForBar sectionSpec barIx))
                          pitchClassPenalty = if topVoice `mod` 12 == anchorPc then 0 else 18
                          overlapPenalty = if topVoice >= anchorKey then 10 else 0
                          spacingPenalty = abs ((anchorKey - topVoice) - 7)
                      in pitchClassPenalty + overlapPenalty + spacingPenalty
            in voiceLeadingScore + melodyScore
  in foldl
       (\prev ix -> choose ix prev (candidatesFor ix))
       []
       [0 .. max 0 barInPhrase]

bassRootMidi ∷ Int → SectionSpec → Int → Int
bassRootMidi octave sectionSpec barInPhrase =
  midiFromPitchClass octave (csRootClass (chordForBar sectionSpec barInPhrase))

bassSupportMidi ∷ Int → SectionSpec → Int → Int
bassSupportMidi octave sectionSpec barInPhrase =
  let chord = chordForBar sectionSpec barInPhrase
      thirdPc = chordThirdPitchClass chord
      fifthPc = chordFifthPitchClass chord
      rootPc = csRootClass chord
      rootMidi = bassRootMidi octave sectionSpec barInPhrase
      supportPc =
        if thirdPc == rootPc
          then fifthPc
          else thirdPc
  in pitchClassMidiAtOrAbove (rootMidi + 2) supportPc

bassUpperMidi ∷ Int → SectionSpec → Int → Int
bassUpperMidi octave sectionSpec barInPhrase =
  let supportMidi = bassSupportMidi octave sectionSpec barInPhrase
      chord = chordForBar sectionSpec barInPhrase
      accentPc = harmonicAccentPitchClassForBar sectionSpec barInPhrase chord
  in pitchClassMidiAtOrAbove (supportMidi + 2) accentPc

bassApproachMidi ∷ Int → SectionSpec → Int → Int
bassApproachMidi octave sectionSpec barInPhrase =
  let currentRoot = bassRootMidi octave sectionSpec barInPhrase
      nextRoot = midiFromPitchClass octave (csRootClass (nextChordForBar sectionSpec barInPhrase))
      candidates = [nextRoot - 2, nextRoot - 1, nextRoot + 1, nextRoot + 2, nextRoot]
  in case sortOn (\note -> (abs (note - currentRoot), abs (note - nextRoot))) candidates of
       note : _ -> note
       [] -> nextRoot

harmonicSparklePattern ∷ Int → Float → Float → Float → SectionSpec → Int → [(Float, Int, Float, Float)]
harmonicSparklePattern octave step sustain baseVelocity sectionSpec patternSeed =
  let beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
      slots = beatGrid step beats
      barInPhrase = patternSeed `mod` max 1 (ssBarsPerPhrase sectionSpec)
      voicing = harmonicVoicingForBar octave sectionSpec barInPhrase
      voiceCount = max 1 (length voicing)
      approach = bassApproachMidi octave sectionSpec barInPhrase
      keyAt ix =
        if ix == length slots - 1 && step <= 0.75
          then approach + 12
          else voicing !! ((ix + patternSeed) `mod` voiceCount)
      velAt ix = clamp01 (baseVelocity + fromIntegral (ix `mod` 3) * 0.04)
  in [ (beat, keyAt ix, sustain, velAt ix)
     | (ix, beat) <- zip [0 :: Int ..] slots
     ]

midiFromPitchClass ∷ Int → Int → Int
midiFromPitchClass octave pitchClass = 12 * (octave + 1) + pitchClass

randSigned ∷ Word64 → Float
randSigned seed = rand01 seed * 2 - 1

compileFallbackDrumNotes ∷ String → SectionSpec → Int → Int → Int → Double → [TimelineNote]
compileFallbackDrumNotes sectionName sectionSpec absoluteBarIx barInPhrase barsPerPhrase framesPerBeat =
  let inst =
        InstrumentPatternSpec
          { ipName = "auto-drums"
          , ipInstrumentId = InstrumentId 9
          , ipAmp = 1
          , ipPan = 0
          , ipVelocityVariation = 0
          , ipPatterns = M.empty
          , ipFills = M.empty
          , ipFillGenerator = Nothing
          }
      beats = max 1 (ssBeatsPerBar sectionSpec)
      sectionN = normalizeSectionName sectionName
      phraseBoundary = isPhraseBoundaryBar barInPhrase barsPerPhrase
      kickBeats =
        if phraseBoundary
          then filter (< fromIntegral beats) [0, 1.5, 2.5]
          else if sectionN == "intro" || isEndingSectionName sectionN
          then [0]
          else if sectionN == "bridge"
                 then [0]
                 else if sectionN == "chorus"
                        then filter (< fromIntegral beats) [0, 1.5, 2]
                        else filter (< fromIntegral beats) [0, 2]
      snareBeats =
        if sectionN == "intro" || isEndingSectionName sectionN
          then if phraseBoundary then [fromIntegral (beats - 1)] else []
          else if beats >= 4
                 then [1, 3]
                 else [fromIntegral (beats - 1)]
      hatStep =
        if sectionN == "chorus"
          then 0.5
          else 1
      hatBeats = beatGrid hatStep (fromIntegral beats)
      mk beat key dur vel =
        PatternNote
          { pnBeatOffset = beat
          , pnNoteKey = NoteKey key
          , pnDuration = dur
          , pnVelocity = vel
          }
      patternNotes =
        map (\b -> mk b 36 0.25 0.9) kickBeats
          <> map (\b -> mk b 38 0.25 0.85) snareBeats
          <> map (\b -> mk b 42 0.12 (if sectionN == "chorus" then 0.58 else 0.45)) hatBeats
          <> if phraseBoundary
               then map (\b -> mk b 47 0.2 0.72) (filter (< fromIntegral beats) [fromIntegral beats - 1, fromIntegral beats - 0.5])
               else []
  in map (compilePatternNote framesPerBeat absoluteBarIx beats inst) patternNotes

generateFillPattern
  ∷ SectionSpec
  → Maybe String
  → Float
  → [TimelineNote]
  → Int
  → Int
  → Int
  → InstrumentPatternSpec
  → [PatternNote]
  → Maybe [PatternNote]
generateFillPattern sectionSpec targetMood targetEnergy prevBarNotes absoluteBarIx barInPhrase barsPerPhrase inst beatNotes
  | not (isDrumInstrumentId (ipInstrumentId inst)) = Nothing
  | not (isPhraseBoundaryBar barInPhrase barsPerPhrase) = Nothing
  | otherwise =
      case ipFillGenerator inst of
        Nothing -> Nothing
        Just cfg ->
          let prevDrumNotes = filter (\n -> tnInstrumentId n == ipInstrumentId inst) prevBarNotes
              contextKeys =
                if null prevDrumNotes
                  then map patternNoteKeyInt beatNotes
                  else map timelineNoteKeyInt prevDrumNotes
              contextDensity =
                if null prevDrumNotes
                  then fromIntegral (length beatNotes)
                  else fromIntegral (length prevDrumNotes)
              beats = fromIntegral (max 1 (ssBeatsPerBar sectionSpec)) ∷ Float
              descWords = fillDescriptorWords targetMood sectionSpec
              energy = clamp01 targetEnergy
              moodDensityBias
                | hasAny descWords ["frantic", "intense", "aggressive", "dense", "heavy"] = 0.14
                | hasAny descWords ["steady", "driving", "pulse"] = 0.08
                | hasAny descWords ["sparse", "minimal", "calm", "ambient"] = -0.10
                | otherwise = 0
              contextBias
                | contextDensity / beats < 3 = 0.12
                | contextDensity / beats > 5.5 = -0.06
                | otherwise = 0
              density = clamp01 (fgDensity cfg + moodDensityBias + contextBias + (energy - 0.5) * 0.20)
              fillLength =
                clampFillLength
                  beats
                  ( fgLengthBeats cfg
                      + if hasAny descWords ["frantic", "intense", "rising", "swell"]
                          then 0.50
                          else 0
                      + if hasAny descWords ["sparse", "minimal"]
                          then -0.50
                          else 0
                  )
              fillStart = max 0 (beats - fillLength)
              subdivision
                | density + fgVariation cfg > 1.10 = 0.25
                | otherwise = 0.50
              styleSeed =
                nextConductorSeed
                  ( fromIntegral absoluteBarIx * 0x9E3779B97F4A7C15
                      `xor` fromIntegral (length contextKeys * 17 + floor (energy * 100))
                  )
              styleWeights =
                [ ("snare-answer", 3 + if countMatching isSnareKey contextKeys > 0 then 2 else 0)
                , ("tom-run", 3 + if hasAny descWords ["frantic", "dense", "intense", "bridge"] then 3 else 0)
                , ("kick-answer", 2 + if countMatching isKickKey contextKeys > 1 then 2 else 0)
                , ("cymbal-lift", 2 + if hasAny descWords ["rising", "swell", "release", "intro"] then 4 else 0)
                ]
              styleName =
                case weightedPick styleSeed styleWeights of
                  Just choice -> wcName choice
                  Nothing -> "snare-answer"
              leadCymbal = chooseLeadCymbalKey descWords contextKeys
              accentCymbal =
                chooseAccentCymbalKey
                  descWords
                  contextKeys
                  (nextConductorSeed styleSeed)
              mainGrid = fillGrid fillStart beats subdivision
              endSnareBeat = max fillStart (beats - max subdivision 0.50)
              kickAnchor =
                if countMatching isKickKey contextKeys > 0 && fillLength > 1.0
                  then [mkPatternNote fillStart 36 0.20 0.72]
                  else []
              leadNote =
                if fillLength >= 0.75
                  then [mkPatternNote fillStart leadCymbal (cymbalDuration leadCymbal) (0.40 + 0.12 * density)]
                  else []
              bodyNotes =
                fillBodyNotes
                  styleName
                  descWords
                  (fgVariation cfg)
                  fillStart
                  endSnareBeat
                  mainGrid
                  styleSeed
                  contextKeys
              ghostNotes =
                fillGhostNotes
                  (fgVariation cfg)
                  fillStart
                  endSnareBeat
                  subdivision
                  (nextConductorSeed (styleSeed + 0x5BF03635))
              finalAccent =
                [ mkPatternNote endSnareBeat 38 0.22 0.90
                , mkPatternNote
                    (min (beats - 0.10) (endSnareBeat + min 0.12 subdivision))
                    accentCymbal
                    (cymbalDuration accentCymbal)
                    (0.66 + 0.18 * density)
                ]
              generated =
                sortOn pnBeatOffset (kickAnchor <> leadNote <> bodyNotes <> ghostNotes <> finalAccent)
          in if null generated then Nothing else Just generated

fillBodyNotes
  ∷ String
  → [String]
  → Float
  → Float
  → Float
  → [Float]
  → Word64
  → [Int]
  → [PatternNote]
fillBodyNotes styleName descWords variation fillStart endSnareBeat grid seed contextKeys =
  let bodySlots = filter (\beat -> beat > fillStart + 0.001 && beat < endSnareBeat - 0.001) grid
      tomLane = chooseTomLane descWords variation seed
      snareChoices = [38, 40]
      keyFor ix =
        case styleName of
          "tom-run" -> tomLane !! (ix `mod` length tomLane)
          "kick-answer" ->
            ([38, 36, 45, 38, 47] !! (ix `mod` 5))
          "cymbal-lift" ->
            ([46, 38, 47, 38, 50] !! (ix `mod` 5))
          _ ->
            let tomKey = tomLane !! (ix `mod` length tomLane)
                snareKey = snareChoices !! (ix `mod` length snareChoices)
            in if even ix then snareKey else tomKey
      velocityFor ix key =
        clamp01
          ( 0.58
              + if isSnareKey key then 0.10 else 0.04
              + if isTomKey key then 0.06 else 0
              + fromIntegral (ix `mod` 3) * 0.04
              + variation * 0.08
          )
      durFor key
        | isCymbalLike key = cymbalDuration key
        | isKickKey key = 0.20
        | otherwise = 0.18 + variation * 0.04
      useBodySlot ix _beat =
        rand01 (seed + fromIntegral (ix * 97 + keyFor ix * 11 + countMatching isKickKey contextKeys))
          <= 0.62 + variation * 0.22
      body =
        [ mkPatternNote beat key (durFor key) (velocityFor ix key)
        | (ix, beat) <- zip [0 :: Int ..] bodySlots
        , let key = keyFor ix
        , useBodySlot ix beat
        ]
  in case (body, bodySlots) of
       ([], beat : _) ->
         let key =
               if styleName == "tom-run"
                 then case tomLane of
                        tom : _ -> tom
                        [] -> 45
                 else 38
         in [mkPatternNote beat key 0.20 0.72]
       _ -> body

fillGhostNotes ∷ Float → Float → Float → Float → Word64 → [PatternNote]
fillGhostNotes variation fillStart endSnareBeat subdivision seed
  | variation < 0.45 = []
  | otherwise =
      let offset = if subdivision <= 0.25 then 0.125 else 0.25
          slots = takeWhile (< endSnareBeat - 0.20) [fillStart + offset, fillStart + offset + max offset subdivision ..]
      in [ mkPatternNote beat 40 0.12 (0.34 + variation * 0.12)
         | (ix, beat) <- zip [0 :: Int ..] slots
         , rand01 (seed + fromIntegral (ix * 131)) <= 0.45 + variation * 0.20
         ]

fillDescriptorWords ∷ Maybe String → SectionSpec → [String]
fillDescriptorWords targetMood spec =
  nub (sectionDescriptorWords spec <> maybe [] wordsLower targetMood)

chooseTomLane ∷ [String] → Float → Word64 → [Int]
chooseTomLane descWords variation seed =
  let baseOptions =
        [ [45, 47, 48, 50]
        , [50, 48, 47, 45]
        , [43, 45, 47, 50]
        ]
      intenseOptions =
        if hasAny descWords ["frantic", "intense", "dense", "aggressive"]
          then [[43, 45, 47, 48, 50], [50, 48, 47, 45, 43]]
          else []
      opts = baseOptions <> intenseOptions
      ix = seededIndex seed (length opts)
      chosen = opts !! ix
  in if variation < 0.30 then take 3 chosen else chosen

chooseLeadCymbalKey ∷ [String] → [Int] → Int
chooseLeadCymbalKey descWords contextKeys =
  case dominantKeyBy isHatLike contextKeys of
    Just 42 -> 46
    Just 44 -> 46
    Just key -> key
    Nothing ->
      case dominantKeyBy isRideLike contextKeys of
        Just key -> key
        Nothing ->
          if hasAny descWords ["rising", "swell", "intro"]
            then 46
            else 42

chooseAccentCymbalKey ∷ [String] → [Int] → Word64 → Int
chooseAccentCymbalKey descWords contextKeys seed =
  let choices
        | hasAny descWords ["frantic", "intense", "aggressive", "dense"] = [52, 57, 49]
        | hasAny descWords ["rising", "swell", "release"] = [57, 55, 49]
        | hasAny descWords ["steady", "driving", "pulse"] = [49, 55, 52]
        | dominantKeyBy isRideLike contextKeys /= Nothing = [49, 55, 57]
        | otherwise = [49, 57, 55]
  in choices !! seededIndex seed (length choices)

fillGrid ∷ Float → Float → Float → [Float]
fillGrid startBeat beats subdivision =
  takeWhile (< beats - 0.001) [startBeat, startBeat + subdivision ..]

clampFillLength ∷ Float → Float → Float
clampFillLength beats x = max 0.75 (min beats x)

mkPatternNote ∷ Float → Int → Float → Float → PatternNote
mkPatternNote beat key dur vel =
  PatternNote
    { pnBeatOffset = beat
    , pnNoteKey = NoteKey key
    , pnDuration = dur
    , pnVelocity = clamp01 vel
    }

cymbalDuration ∷ Int → Float
cymbalDuration key
  | key `elem` [42, 44, 46] = 0.24
  | key `elem` [51, 53, 59] = 0.42
  | key == 55 = 0.72
  | otherwise = 1.05

timelineNoteKeyInt ∷ TimelineNote → Int
timelineNoteKeyInt note =
  case tnKey note of
    NoteKey key -> key

patternNoteKeyInt ∷ PatternNote → Int
patternNoteKeyInt note =
  case pnNoteKey note of
    NoteKey key -> key

countMatching ∷ (Int → Bool) → [Int] → Int
countMatching predFn = length . filter predFn

dominantKeyBy ∷ (Int → Bool) → [Int] → Maybe Int
dominantKeyBy predFn keys =
  case sortOn (\(count, key) -> (-count, key)) (mapMaybe asCount (M.toList counts)) of
    (_, key) : _ -> Just key
    [] -> Nothing
  where
    counts = M.fromListWith (+) [(key, 1 :: Int) | key <- keys, predFn key]
    asCount (key, count)
      | count > 0 = Just (count, key)
      | otherwise = Nothing

seededIndex ∷ Word64 → Int → Int
seededIndex _ n | n <= 1 = 0
seededIndex seed n = fromIntegral (seed `mod` fromIntegral n)

hasAny ∷ [String] → [String] → Bool
hasAny haystack needles = any (`elem` haystack) needles

isKickKey, isSnareKey, isHatLike, isRideLike, isTomKey, isCymbalLike ∷ Int → Bool
isKickKey key = key `elem` [35, 36]
isSnareKey key = key `elem` [38, 39, 40]
isHatLike key = key `elem` [42, 44, 46]
isRideLike key = key `elem` [51, 53, 59]
isTomKey key = key `elem` [41, 43, 45, 47, 48, 50]
isCymbalLike key = key `elem` [42, 44, 46, 49, 51, 52, 53, 55, 57, 59]

isPhraseBoundaryBar ∷ Int → Int → Bool
isPhraseBoundaryBar barInPhrase barsPerPhrase =
  barsPerPhrase > 0 && barInPhrase == (barsPerPhrase - 1)

beatGrid ∷ Float → Float → [Float]
beatGrid step beats
  | step <= 0 = []
  | otherwise = takeWhile (< beats) [0, step ..]

isDrumInstrumentId ∷ InstrumentId → Bool
isDrumInstrumentId iid =
  case iid of
    InstrumentId n -> n == 9

isChorusSection ∷ SectionSpec → Bool
isChorusSection spec = isChorusSectionName (ssName spec)

isBridgeSection ∷ SectionSpec → Bool
isBridgeSection spec = isBridgeSectionName (ssName spec)

compilePatternNote ∷ Double → Int → Int → InstrumentPatternSpec → PatternNote → TimelineNote
compilePatternNote framesPerBeat absoluteBarIx beatsPerBar inst note =
  let baseOnFrames = floor (realToFrac (pnBeatOffset note) * framesPerBeat) ∷ Integer
      timingJitterFrames = humanizedTimingJitterFrames framesPerBeat absoluteBarIx beatsPerBar inst note
      onFrames = max 0 (baseOnFrames + timingJitterFrames)
      durFrames = max 1 (floor (realToFrac (pnDuration note) * framesPerBeat) ∷ Integer)
      onOffset = fromIntegral onFrames
      offOffset = fromIntegral (max (onFrames + 1) (onFrames + durFrames))
      velocity = humanizedVelocity absoluteBarIx beatsPerBar inst note
  in TimelineNote
      { tnInstrumentId = ipInstrumentId inst
      , tnAmp = max 0 (ipAmp inst)
      , tnPan = max (-1) (min 1 (ipPan inst))
      , tnKey = pnNoteKey note
      , tnVelocity = velocity
      , tnOnOffsetFrames = onOffset
      , tnOffOffsetFrames = offOffset
      }

humanizedVelocity ∷ Int → Int → InstrumentPatternSpec → PatternNote → Float
humanizedVelocity absoluteBarIx beatsPerBar inst note =
  let base = clamp01 (pnVelocity note)
      variation = clamp01 (ipVelocityVariation inst)
  in if variation <= 0
       then base
       else
         let beatPhase = wrapBeatPhase (pnBeatOffset note) (max 1 beatsPerBar)
             accentBoost
               | isNearBeat beatPhase 0 = 0.20 * variation
               | isNearBeat beatPhase 2 = 0.12 * variation
               | otherwise = 0
             seed =
               velocitySeed
                 absoluteBarIx
                 (ipInstrumentId inst)
                 (pnNoteKey note)
                 beatPhase
             jitter = (rand01 seed - 0.5) * 0.10 * variation
         in clamp01 (base + accentBoost + jitter)

humanizedTimingJitterFrames ∷ Double → Int → Int → InstrumentPatternSpec → PatternNote → Integer
humanizedTimingJitterFrames framesPerBeat absoluteBarIx beatsPerBar inst note =
  let variation = clamp01 (ipVelocityVariation inst)
  in if variation <= 0 || not (isDrumInstrumentId (ipInstrumentId inst))
       then 0
       else
         let beatPhase = wrapBeatPhase (pnBeatOffset note) (max 1 beatsPerBar)
             NoteKey key = pnNoteKey note
             jitterBeats
               | key `elem` [42, 44, 46, 49, 51, 52, 55, 57, 59] = 0.030 * variation
               | key `elem` [38, 39, 40] = 0.014 * variation
               | otherwise = 0.010 * variation
             seed =
               timingSeed
                 absoluteBarIx
                 (ipInstrumentId inst)
                 (pnNoteKey note)
                 beatPhase
             jitter = (rand01 seed - 0.5) * 2 * jitterBeats
         in round (realToFrac jitter * framesPerBeat)

wrapBeatPhase ∷ Float → Int → Float
wrapBeatPhase beatOffset beatsPerBar =
  let beats = fromIntegral (max 1 beatsPerBar) ∷ Float
      q = fromIntegral (floor (beatOffset / beats) ∷ Int) ∷ Float
      wrapped = beatOffset - q * beats
  in if wrapped < 0 then wrapped + beats else wrapped

isNearBeat ∷ Float → Float → Bool
isNearBeat x target = abs (x - target) <= 0.001

velocitySeed ∷ Int → InstrumentId → NoteKey → Float → Word64
velocitySeed absoluteBarIx iid key beatPhase =
  let beatTicks = floor (beatPhase * 960) ∷ Int
      InstrumentId instN = iid
      NoteKey noteN = key
      seedBase =
        fromIntegral absoluteBarIx * 0x9E3779B97F4A7C15
          + fromIntegral (instN * 131 + noteN * 17 + beatTicks * 7)
  in nextConductorSeed seedBase

timingSeed ∷ Int → InstrumentId → NoteKey → Float → Word64
timingSeed absoluteBarIx iid key beatPhase =
  velocitySeed absoluteBarIx iid key beatPhase + 0xA24BAED4963EE407

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

arrangeGeneratedInstruments ∷ String → Map String SectionSpec → Either String [InstrumentPatternSpec]
arrangeGeneratedInstruments genre sections
  | not (any sectionNeedsArrangement (M.elems sections)) = pure []
  | null genre = Left "song.genre is required when using section chords/melody without explicit instruments"
  | normalizeName genre == "electronic" =
      pure
        [ generatedInstrument "drums" 9 1.0 0 0.7 (Just (FillGeneratorSpec 0.62 0.74 2.0))
        , generatedInstrument "bass" 38 0.88 0 0.22 Nothing
        , generatedInstrument "pad" 88 0.52 0 0.10 Nothing
        , generatedInstrument "arp" 81 0.46 0.12 0.18 Nothing
        , generatedInstrument "lead" 80 0.86 (-0.05) 0.12 Nothing
        ]
  | normalizeName genre == "ambient" =
      pure
        [ generatedInstrument "drums" 9 0.76 0 0.24 (Just (FillGeneratorSpec 0.30 0.24 1.0))
        , generatedInstrument "bass" 39 0.54 0 0.10 Nothing
        , generatedInstrument "pad" 94 0.68 0 0.04 Nothing
        , generatedInstrument "arp" 98 0.32 0.10 0.06 Nothing
        , generatedInstrument "lead" 88 0.38 0 0.04 Nothing
        ]
  | normalizeName genre == "blackmetal" =
      pure
        [ generatedInstrument "drums" 9 1.0 0 0.58 (Just (FillGeneratorSpec 0.78 0.84 2.0))
        , generatedInstrument "bass" 34 0.92 0 0.18 Nothing
        , generatedInstrument "pad" 48 0.58 0 0.08 Nothing
        , generatedInstrument "arp" 30 0.84 (-0.08) 0.14 Nothing
        , generatedInstrument "lead" 31 0.90 0.08 0.12 Nothing
        ]
  | normalizeName genre == "cinematic" =
      pure
        [ generatedInstrument "drums" 9 0.96 0 0.38 (Just (FillGeneratorSpec 0.48 0.54 1.5))
        , generatedInstrument "bass" 43 0.82 0 0.12 Nothing
        , generatedInstrument "pad" 91 0.62 0 0.06 Nothing
        , generatedInstrument "arp" 48 0.44 0.08 0.10 Nothing
        , generatedInstrument "lead" 61 0.78 0 0.08 Nothing
        ]
  | otherwise = Left ("unsupported song.genre: " <> genre <> " (expected electronic|ambient|blackmetal|cinematic)")
  where
    sectionNeedsArrangement spec =
      not (null (ssChordBars spec)) || not (null (ssMelodyPhrase spec))
    generatedInstrument name iid amp pan velocityVariation fillGenerator =
      InstrumentPatternSpec
        { ipName = name
        , ipInstrumentId = InstrumentId iid
        , ipAmp = amp
        , ipPan = pan
        , ipVelocityVariation = velocityVariation
        , ipPatterns = M.empty
        , ipFills = M.empty
        , ipFillGenerator = fillGenerator
        }

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

lastTimelineBar ∷ [TimelineBar] → Maybe TimelineBar
lastTimelineBar bars =
  case reverse bars of
    bar : _ -> Just bar
    [] -> Nothing
