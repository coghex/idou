{-# LANGUAGE Strict, UnicodeSyntax #-}

module Perf.Baseline
  ( BaselineScenario(..)
  , BaselineScenarioStep(..)
  , BaselineScenarioAction(..)
  , ScheduledBaselineAction(..)
  , defaultBaselineScenario
  , baselineScenarioNames
  , lookupBaselineScenario
  , scheduleBaselineScenario
  , timelineAbsoluteBarStartFrames
  , LookaheadPerfSummary(..)
  , summarizeLookaheadTelemetry
  , lookaheadAverageGenerationUs
  , lookaheadAverageBarsPerSecond
  , timelineAbsoluteEndFrameForBars
  ) where

import Data.Char (toLower)
import Data.List (find)
import Data.Word (Word32, Word64)

import qualified Data.Map.Strict as M

import Player.Timeline.Explain (TimelineExplainEvent(..), drainTimelineExplainEvents)
import Player.Timeline.Types (SongSpec, TimelineBar(..), TimelineLookaheadTelemetry(..))

data BaselineScenarioAction
  = BaselineSetGenre !(Maybe String)
  | BaselineSetMood !(Maybe String)
  | BaselineSetEnergy !Float
  deriving (Eq, Show)

data BaselineScenarioStep = BaselineScenarioStep
  { bssBarIndex ∷ !Int
  , bssAction   ∷ !BaselineScenarioAction
  }
  deriving (Eq, Show)

data BaselineScenario = BaselineScenario
  { bsName        ∷ !String
  , bsDescription ∷ !String
  , bsDefaultBars ∷ !Int
  , bsSteps       ∷ ![BaselineScenarioStep]
  }
  deriving (Eq, Show)

data ScheduledBaselineAction = ScheduledBaselineAction
  { sbaBarIndex ∷ !Int
  , sbaFrame    ∷ !Word64
  , sbaAction   ∷ !BaselineScenarioAction
  }
  deriving (Eq, Show)

data LookaheadPerfSummary = LookaheadPerfSummary
  { lpsEventCount           ∷ !Int
  , lpsGeneratedBars        ∷ !Int
  , lpsMeasuredEventCount   ∷ !Int
  , lpsTotalGenerationUs    ∷ !Word64
  , lpsPeakGenerationUs     ∷ !Word64
  , lpsTotalBarsPerSecond   ∷ !Float
  , lpsPeakBarsPerSecond    ∷ !Float
  , lpsMinBarsPerSecond     ∷ !(Maybe Float)
  , lpsReasonCounts         ∷ ![(String, Int)]
  }
  deriving (Eq, Show)

defaultBaselineScenario ∷ BaselineScenario
defaultBaselineScenario = steadySongScenario

baselineScenarioNames ∷ [String]
baselineScenarioNames = map bsName builtinBaselineScenarios

lookupBaselineScenario ∷ String → Maybe BaselineScenario
lookupBaselineScenario rawName =
  let lowered = map toLower rawName
  in find (\scenario -> map toLower (bsName scenario) == lowered) builtinBaselineScenarios

scheduleBaselineScenario ∷ Word32 → Word64 → Int → SongSpec → BaselineScenario → [ScheduledBaselineAction]
scheduleBaselineScenario sampleRateHz startFrame requestedBars spec scenario =
  case timelineAbsoluteBarStartFrames sampleRateHz startFrame requestedBars spec of
    Nothing -> []
    Just startFrames ->
      [ ScheduledBaselineAction
          { sbaBarIndex = bssBarIndex step
          , sbaFrame = frame
          , sbaAction = bssAction step
          }
      | step <- bsSteps scenario
      , bssBarIndex step >= 0
      , bssBarIndex step < requestedBars
      , Just frame <- [lookupFrame (bssBarIndex step) startFrames]
      ]
  where
    lookupFrame ix frames =
      case drop ix frames of
        frame : _ -> Just frame
        [] -> Nothing

data LookaheadAccum = LookaheadAccum
  { laCount              ∷ !Int
  , laGeneratedBars      ∷ !Int
  , laMeasuredCount      ∷ !Int
  , laTotalUs            ∷ !Word64
  , laPeakUs             ∷ !Word64
  , laTotalBarsPerSecond ∷ !Float
  , laPeakBarsPerSecond  ∷ !Float
  , laMinBarsPerSecond   ∷ !(Maybe Float)
  , laReasonMap          ∷ !(M.Map String Int)
  }

emptyAccum ∷ LookaheadAccum
emptyAccum = LookaheadAccum 0 0 0 0 0 0 0 Nothing M.empty

summarizeLookaheadTelemetry ∷ [TimelineLookaheadTelemetry] → LookaheadPerfSummary
summarizeLookaheadTelemetry telemetries =
  let acc = foldl' step emptyAccum telemetries
  in
    LookaheadPerfSummary
      { lpsEventCount = laCount acc
      , lpsGeneratedBars = laGeneratedBars acc
      , lpsMeasuredEventCount = laMeasuredCount acc
      , lpsTotalGenerationUs = laTotalUs acc
      , lpsPeakGenerationUs = laPeakUs acc
      , lpsTotalBarsPerSecond = laTotalBarsPerSecond acc
      , lpsPeakBarsPerSecond = laPeakBarsPerSecond acc
      , lpsMinBarsPerSecond = laMinBarsPerSecond acc
      , lpsReasonCounts = M.toAscList (laReasonMap acc)
      }
  where
    step acc telemetry =
      let acc' = acc
            { laCount = laCount acc + 1
            , laGeneratedBars = laGeneratedBars acc + tltGeneratedBarCount telemetry
            , laReasonMap = M.insertWith (+) (tltReason telemetry) 1 (laReasonMap acc)
            }
      in
        case (tltGenerationDurationUs telemetry, tltGenerationBarsPerSecond telemetry) of
          (Just durationUs, Just barsPerSecond) ->
            acc'
              { laMeasuredCount = laMeasuredCount acc + 1
              , laTotalUs = laTotalUs acc + durationUs
              , laPeakUs = max (laPeakUs acc) durationUs
              , laTotalBarsPerSecond = laTotalBarsPerSecond acc + barsPerSecond
              , laPeakBarsPerSecond = max (laPeakBarsPerSecond acc) barsPerSecond
              , laMinBarsPerSecond = Just (maybe barsPerSecond (min barsPerSecond) (laMinBarsPerSecond acc))
              }
          (Just durationUs, Nothing) ->
            acc'
              { laMeasuredCount = laMeasuredCount acc + 1
              , laTotalUs = laTotalUs acc + durationUs
              , laPeakUs = max (laPeakUs acc) durationUs
              }
          _ -> acc'

lookaheadAverageGenerationUs ∷ LookaheadPerfSummary → Maybe Word64
lookaheadAverageGenerationUs summary
  | lpsMeasuredEventCount summary <= 0 = Nothing
  | otherwise =
      Just (lpsTotalGenerationUs summary `div` fromIntegral (lpsMeasuredEventCount summary))

lookaheadAverageBarsPerSecond ∷ LookaheadPerfSummary → Maybe Float
lookaheadAverageBarsPerSecond summary
  | lpsMeasuredEventCount summary <= 0 = Nothing
  | otherwise =
      Just (lpsTotalBarsPerSecond summary / fromIntegral (lpsMeasuredEventCount summary))

timelineAbsoluteBarStartFrames ∷ Word32 → Word64 → Int → SongSpec → Maybe [Word64]
timelineAbsoluteBarStartFrames sampleRateHz startFrame requestedBars spec
  | requestedBars <= 0 = Nothing
  | otherwise =
      case drainTimelineExplainEvents sampleRateHz startFrame spec of
        Left _ -> Nothing
        Right events ->
          let bars = [ bar | TimelineExplainBar bar <- events ]
          in case take requestedBars bars of
               selectedBars
                 | length selectedBars == requestedBars ->
                     Just [startFrame + tbStartOffsetFrames bar | bar <- selectedBars]
               _ -> Nothing

timelineAbsoluteEndFrameForBars ∷ Word32 → Word64 → Int → SongSpec → Maybe Word64
timelineAbsoluteEndFrameForBars sampleRateHz startFrame requestedBars spec
  | requestedBars <= 0 = Nothing
  | otherwise =
      case drainTimelineExplainEvents sampleRateHz startFrame spec of
        Left _ -> Nothing
        Right events ->
          let bars = [ bar | TimelineExplainBar bar <- events ]
          in case take requestedBars bars of
               wantedBars
                 | length wantedBars == requestedBars ->
                     case reverse wantedBars of
                       bar : _ -> Just (startFrame + tbStartOffsetFrames bar + tbLengthFrames bar)
                       [] -> Nothing
               _ -> Nothing

builtinBaselineScenarios ∷ [BaselineScenario]
builtinBaselineScenarios =
  [ steadySongScenario
  , rapidRetargetScenario
  , genreSweepScenario
  , longLookaheadScenario
  ]

steadySongScenario ∷ BaselineScenario
steadySongScenario =
  BaselineScenario
    { bsName = "steady-song"
    , bsDescription = "No live retargeting; measure steady playback cost."
    , bsDefaultBars = 16
    , bsSteps = []
    }

rapidRetargetScenario ∷ BaselineScenario
rapidRetargetScenario =
  BaselineScenario
    { bsName = "rapid-retarget"
    , bsDescription = "Change mood, energy, and genre on successive bars to stress future-generated retargeting."
    , bsDefaultBars = 12
    , bsSteps =
        [ BaselineScenarioStep 1 (BaselineSetEnergy 0.2)
        , BaselineScenarioStep 2 (BaselineSetMood (Just "calm"))
        , BaselineScenarioStep 3 (BaselineSetGenre (Just "ambient"))
        , BaselineScenarioStep 4 (BaselineSetEnergy 0.9)
        , BaselineScenarioStep 5 (BaselineSetMood (Just "combat"))
        , BaselineScenarioStep 6 (BaselineSetGenre (Just "cinematic"))
        , BaselineScenarioStep 7 (BaselineSetEnergy 0.35)
        , BaselineScenarioStep 8 (BaselineSetMood Nothing)
        , BaselineScenarioStep 9 (BaselineSetGenre (Just "blackmetal"))
        , BaselineScenarioStep 10 (BaselineSetEnergy 0.75)
        , BaselineScenarioStep 11 (BaselineSetGenre Nothing)
        ]
    }

genreSweepScenario ∷ BaselineScenario
genreSweepScenario =
  BaselineScenario
    { bsName = "genre-sweep"
    , bsDescription = "Cycle through generated palettes every two bars."
    , bsDefaultBars = 16
    , bsSteps =
        [ BaselineScenarioStep 2 (BaselineSetGenre (Just "ambient"))
        , BaselineScenarioStep 4 (BaselineSetGenre (Just "cinematic"))
        , BaselineScenarioStep 6 (BaselineSetGenre (Just "blackmetal"))
        , BaselineScenarioStep 8 (BaselineSetGenre (Just "electronic"))
        , BaselineScenarioStep 10 (BaselineSetGenre (Just "ambient"))
        , BaselineScenarioStep 12 (BaselineSetGenre (Just "cinematic"))
        , BaselineScenarioStep 14 (BaselineSetGenre Nothing)
        ]
    }

longLookaheadScenario ∷ BaselineScenario
longLookaheadScenario =
  BaselineScenario
    { bsName = "long-lookahead"
    , bsDescription = "Long steady run for accumulation and drift checks."
    , bsDefaultBars = 32
    , bsSteps = []
    }
