{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline
  ( SongMode(..)
  , PatternNote(..)
  , FillGeneratorSpec(..)
  , ChordSpec(..)
  , ChordRegionSpec(..)
  , InstrumentPatternSpec(..)
  , SectionSpec(..)
  , SongSpec(..)
  , TimelineNote(..)
  , TimelineBar(..)
  , TimelineRetargetTelemetry(..)
  , TimelineLookaheadTelemetry(..)
  , TimelineTransitionTelemetry(..)
  , TimelineRuntime(..)
  , loadSongSpec
  , parseSongSpecText
  , cueSectionOrder
  , compileTimelineBars
  , prepareTimelineRuntime
  , setTimelineTargets
  , popReadyBars
  , popRetargetTelemetry
  , popLookaheadTelemetry
  , popTransitionTelemetry
  , setTimelineGenre
  , validateSongGenre
  , generatedInstrumentSpecs
  , lookupGeneratedInstrument
  , timelineNextBarBoundaryFrame
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  ) where

import Player.Timeline.Arrangement (compileTimelineBars, cueSectionOrder)
import Player.Timeline.Parse
  ( generatedInstrumentSpecs
  , loadSongSpec
  , lookupGeneratedInstrument
  , parseSongSpecText
  , validateSongGenre
  )
import Player.Timeline.Runtime
  ( popReadyBars
  , popRetargetTelemetry
  , popLookaheadTelemetry
  , popTransitionTelemetry
  , prepareTimelineRuntime
  , setTimelineGenre
  , setTimelineTargets
  , timelineBoundarySpanFromNextBar
  , timelineNextBarBoundaryFrame
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  )
import Player.Timeline.Types
  ( ChordRegionSpec(..)
  , ChordSpec(..)
  , FillGeneratorSpec(..)
  , InstrumentPatternSpec(..)
  , PatternNote(..)
  , SectionSpec(..)
  , SongMode(..)
  , SongSpec(..)
  , TimelineBar(..)
  , TimelineLookaheadTelemetry(..)
  , TimelineNote(..)
  , TimelineRetargetTelemetry(..)
  , TimelineRuntime(..)
  , TimelineTransitionTelemetry(..)
  )
