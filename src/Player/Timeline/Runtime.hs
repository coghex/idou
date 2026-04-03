{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline.Runtime
  ( prepareTimelineRuntime
  , setTimelineTargets
  , popReadyBars
  , popRetargetTelemetry
  , popLookaheadTelemetry
  , popTransitionTelemetry
  , stampLookaheadPerfMetrics
  , setTimelineGenre
  , timelineNextBarBoundaryFrame
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  ) where

import Data.Bits (xor)
import Data.Char (isSpace, toLower)
import Data.Foldable (toList)
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import Data.Word (Word32, Word64)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Audio.Types (InstrumentId(..), NoteKey(..))

import Player.Timeline.Arrangement
  ( compileSectionNotes
  , cueSectionOrder
  , sectionBarFrames
  , sectionHasArrangement
  )
import Player.Timeline.Parse
  ( arrangeGeneratedInstruments
  , instrumentPatchOverrides
  , validateSongGenre
  )
import Player.Timeline.Types

prepareTimelineRuntime ∷ Word32 → Word64 → SongSpec → TimelineRuntime
prepareTimelineRuntime sampleRateHz startFrame spec =
  let firstSection = initialSectionName spec
      seed0 = startFrame * 0x9E3779B97F4A7C15 + fromIntegral (length (sgSections spec))
  in TimelineRuntime
       { trBars = Seq.empty
       , trStartFrame = startFrame
       , trLookaheadBars = max 1 (sgLookaheadBars spec)
       , trNextBarIx = 0
       , trBarOffset = 0
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
        , trPendingRetargets = []
        , trPendingLookahead = []
        , trPendingTransitions = []
        , trDone = M.null (sgSections spec)
        }

setTimelineTargets ∷ Maybe String → Float → TimelineRuntime → TimelineRuntime
setTimelineTargets mood energy rt =
  let mood' = normalizeMood mood
      energy' = clamp01 energy
      rt' =
        rt
          { trTargetMood = mood'
          , trTargetEnergy = energy'
          }
  in if trTargetMood rt == mood' && trTargetEnergy rt == energy'
       then rt'
       else appendRetargetTelemetry "future-generated-bars-only" rt'

setTimelineGenre ∷ String → TimelineRuntime → Either String TimelineRuntime
setTimelineGenre genreRaw rt = do
  genre <- validateSongGenre genreRaw
  instruments <-
    if any sectionHasArrangement (M.elems (trSections rt))
      then arrangeGeneratedInstruments genre (instrumentPatchOverrides (trInstruments rt)) (trSections rt)
      else pure (trInstruments rt)
  pure
    (if trSongGenre rt == genre
       then rt { trSongGenre = genre, trInstruments = instruments }
       else
         appendRetargetTelemetry
           "future-generated-bars-only"
           ( rt
               { trSongGenre = genre
               , trInstruments = instruments
               }
           ))

popReadyBars ∷ Word64 → TimelineRuntime → ([TimelineBar], TimelineRuntime)
popReadyBars now runtime =
  let rt0 = ensureGeneratedToHorizon "lookahead-horizon" now runtime
      horizon = now + fromIntegral (trLookaheadBars rt0) * currentSectionBarFrames rt0
  in go horizon [] rt0
  where
    go horizon acc rt =
      case nextBar rt of
        Nothing -> (reverse acc, dropConsumedBars rt)
        Just bar ->
          let barStartAbs = trStartFrame rt + tbStartOffsetFrames bar
          in if barStartAbs <= horizon
               then
                 go
                   horizon
                   (bar : acc)
                   rt { trNextBarIx = trNextBarIx rt + 1 }
               else
                 (reverse acc, dropConsumedBars rt)

timelineRuntimeDone ∷ TimelineRuntime → Bool
timelineRuntimeDone rt = trDone rt && trNextBarIx rt >= trBarOffset rt + Seq.length (trBars rt)

timelineRuntimeEndFrame ∷ TimelineRuntime → Maybe Word64
timelineRuntimeEndFrame rt =
  case Seq.viewr (trBars rt) of
    Seq.EmptyR -> Nothing
    _ Seq.:> bar ->
      Just (trStartFrame rt + tbStartOffsetFrames bar + tbLengthFrames bar)

timelineNextBarBoundaryFrame ∷ Word64 → TimelineRuntime → (Maybe Word64, TimelineRuntime)
timelineNextBarBoundaryFrame now rt0 =
  let rt = ensureFutureBarStarts "next-bar-boundary-query" now 1 rt0
  in (firstFutureBoundary now rt, rt)

timelineBoundarySpanFromNextBar ∷ Word64 → Int → TimelineRuntime → (Maybe (Word64, Word64), TimelineRuntime)
timelineBoundarySpanFromNextBar now barsFromNext rt0 =
  let bars' = max 0 barsFromNext
      rt = ensureFutureBarStarts "next-boundary-span-query" now (bars' + 1) rt0
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

popRetargetTelemetry ∷ TimelineRuntime → ([TimelineRetargetTelemetry], TimelineRuntime)
popRetargetTelemetry rt =
  (reverse (trPendingRetargets rt), rt { trPendingRetargets = [] })

popLookaheadTelemetry ∷ TimelineRuntime → ([TimelineLookaheadTelemetry], TimelineRuntime)
popLookaheadTelemetry rt =
  (reverse (trPendingLookahead rt), rt { trPendingLookahead = [] })

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
  let seqIx = max 0 (trNextBarIx rt - trBarOffset rt)
      futureBars = Seq.drop seqIx (trBars rt)
  in [ frame
     | bar <- toList futureBars
     , let frame = trStartFrame rt + tbStartOffsetFrames bar
     , frame > now
     ]

ensureFutureBarStarts ∷ String → Word64 → Int → TimelineRuntime → TimelineRuntime
ensureFutureBarStarts reason now needed rt0 =
  appendLookaheadTelemetry reason rt0 (go rt0)
  where
    go rt
      | needed <= length (futureBarStartFrames now rt) = rt
      | trDone rt = rt
      | otherwise = go (appendGeneratedBar rt)

ensureGeneratedToHorizon ∷ String → Word64 → TimelineRuntime → TimelineRuntime
ensureGeneratedToHorizon reason now runtime0 = appendLookaheadTelemetry reason runtime0 (go runtime0)
  where
    go rt
      | trDone rt = rt
      | trStartFrame rt + trNextBarOffset rt > generationHorizon now rt = rt
      | otherwise = go (appendGeneratedBar rt)

appendRetargetTelemetry ∷ String → TimelineRuntime → TimelineRuntime
appendRetargetTelemetry policy rt =
  let event =
        TimelineRetargetTelemetry
          { trtReason = "live-retarget"
          , trtPolicy = policy
          , trtBufferedFutureBars = futureBufferedBarsCount rt
          , trtNextGeneratedBarIx = trGeneratedBarCount rt
          , trtNextGeneratedFrame = trStartFrame rt + trNextBarOffset rt
          , trtMoodTarget = trTargetMood rt
          , trtEnergyTarget = trTargetEnergy rt
          , trtGenre = trSongGenre rt
          }
  in rt { trPendingRetargets = cappedPrepend event (trPendingRetargets rt) }

appendLookaheadTelemetry ∷ String → TimelineRuntime → TimelineRuntime → TimelineRuntime
appendLookaheadTelemetry reason before after =
  let generatedCount = trGeneratedBarCount after - trGeneratedBarCount before
  in if generatedCount <= 0
       then after
       else
         let event =
               TimelineLookaheadTelemetry
                 { tltReason = reason
                 , tltGeneratedBarCount = generatedCount
                 , tltFirstGeneratedBarIx = trGeneratedBarCount before
                 , tltLastGeneratedBarIx = trGeneratedBarCount after - 1
                 , tltStartFrame = trStartFrame before + trNextBarOffset before
                 , tltEndFrame = trStartFrame after + trNextBarOffset after
                 , tltBufferedFutureBars = futureBufferedBarsCount after
                  , tltMoodTarget = trTargetMood after
                  , tltEnergyTarget = trTargetEnergy after
                  , tltGenre = trSongGenre after
                  , tltSeedStart = trRngState before
                  , tltSeedEnd = trRngState after
                  , tltGenerationDurationUs = Nothing
                  , tltGenerationBarsPerSecond = Nothing
                  }
          in after { trPendingLookahead = cappedPrepend event (trPendingLookahead after) }

stampLookaheadPerfMetrics ∷ Word64 → TimelineLookaheadTelemetry → TimelineLookaheadTelemetry
stampLookaheadPerfMetrics durationUs telemetry =
  let barsPerSecond
        | durationUs <= 0 = Nothing
        | otherwise =
            Just
              ( (fromIntegral (tltGeneratedBarCount telemetry) * 1000000)
                  / fromIntegral durationUs
              )
  in
    telemetry
      { tltGenerationDurationUs = Just durationUs
      , tltGenerationBarsPerSecond = barsPerSecond
      }

futureBufferedBarsCount ∷ TimelineRuntime → Int
futureBufferedBarsCount rt =
  let seqIx = max 0 (trNextBarIx rt - trBarOffset rt)
  in max 0 (Seq.length (Seq.drop seqIx (trBars rt)))

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
              { trBars = trBars rt |> bar
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
            , trPendingTransitions = cappedPrepend (tcTelemetry choice) (trPendingTransitions rt)
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

isDrumInstrumentId ∷ InstrumentId → Bool
isDrumInstrumentId iid =
  case iid of
    InstrumentId n -> n == 9

isChorusSection ∷ SectionSpec → Bool
isChorusSection spec = isChorusSectionName (ssName spec)

isBridgeSection ∷ SectionSpec → Bool
isBridgeSection spec = isBridgeSectionName (ssName spec)

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
  case cueSectionOrder spec of
    name : _ | M.member name (sgSections spec) -> name
    _ ->
      case M.keys (sgSections spec) of
        name : _ -> name
        [] -> ""

nextBar ∷ TimelineRuntime → Maybe TimelineBar
nextBar rt =
  let seqIx = max 0 (trNextBarIx rt - trBarOffset rt)
  in Seq.lookup seqIx (trBars rt)

dropConsumedBars ∷ TimelineRuntime → TimelineRuntime
dropConsumedBars rt =
  let consumed = trNextBarIx rt - trBarOffset rt
      seqLen  = Seq.length (trBars rt)
      keep    = if trDone rt then 1 else 0
      toDrop  = max 0 (min consumed (seqLen - keep))
  in if toDrop <= 0
       then rt
       else rt { trBars = Seq.drop toDrop (trBars rt)
               , trBarOffset = trBarOffset rt + toDrop
               }

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

maxPendingTelemetry ∷ Int
maxPendingTelemetry = 256

cappedPrepend ∷ a → [a] → [a]
cappedPrepend x xs = x : take (maxPendingTelemetry - 1) xs

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse


nonEmptyText ∷ String → Maybe String
nonEmptyText raw =
  let stripped = trim raw
  in if null stripped then Nothing else Just stripped

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

lastTimelineBar ∷ Seq TimelineBar → Maybe TimelineBar
lastTimelineBar bars =
  case Seq.viewr bars of
    Seq.EmptyR -> Nothing
    _ Seq.:> bar -> Just bar
