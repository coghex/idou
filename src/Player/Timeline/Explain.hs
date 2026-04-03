{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline.Explain
  ( TimelineExplainEvent(..)
  , drainTimelineExplainEvents
  , barInstrumentNoteCounts
  ) where

import Data.Word (Word32, Word64)

import qualified Data.Map.Strict as M

import Audio.Types (InstrumentId(..))
import Player.Timeline.Runtime
  ( popLookaheadTelemetry
  , popReadyBars
  , popRetargetTelemetry
  , popTransitionTelemetry
  , prepareTimelineRuntime
  , timelineRuntimeDone
  )
import Player.Timeline.Types
  ( SongSpec
  , TimelineBar(..)
  , TimelineLookaheadTelemetry
  , TimelineRetargetTelemetry
  , TimelineTransitionTelemetry
  , TimelineRuntime
  , TimelineNote(..)
  )

data TimelineExplainEvent
  = TimelineExplainRetarget !TimelineRetargetTelemetry
  | TimelineExplainLookahead !TimelineLookaheadTelemetry
  | TimelineExplainTransition !TimelineTransitionTelemetry
  | TimelineExplainBar !TimelineBar
  deriving (Eq, Show)

drainTimelineExplainEvents ∷ Word32 → Word64 → SongSpec → Either String [TimelineExplainEvent]
drainTimelineExplainEvents sampleRateHz startFrame spec =
  go startFrame 0 (prepareTimelineRuntime sampleRateHz startFrame spec) []
  where
    go ∷ Word64 → Int → TimelineRuntime → [TimelineExplainEvent] → Either String [TimelineExplainEvent]
    go now loops rt acc
      | loops > 4096 = Left "timeline explain trace did not finish within 4096 iterations"
      | otherwise =
          let (bars, rt1) = popReadyBars now rt
              (retargets, rt2) = popRetargetTelemetry rt1
              (lookaheads, rt3) = popLookaheadTelemetry rt2
              (transitions, rt4) = popTransitionTelemetry rt3
              chunk =
                map TimelineExplainRetarget retargets
                  <> map TimelineExplainLookahead lookaheads
                  <> map TimelineExplainTransition transitions
                  <> map TimelineExplainBar bars
              acc' = acc <> chunk
              nextNow =
                case reverse bars of
                  bar : _ -> startFrame + tbStartOffsetFrames bar + tbLengthFrames bar
                  [] -> now + 120000
          in
            if null chunk && timelineRuntimeDone rt4
              then Right acc'
              else go nextNow (loops + 1) rt4 acc'

barInstrumentNoteCounts ∷ TimelineBar → [(Int, Int)]
barInstrumentNoteCounts bar =
  M.toAscList $
    M.fromListWith (+)
      [ (iid, 1 ∷ Int)
      | note <- tbNotes bar
      , let InstrumentId iid = tnInstrumentId note
      ]
