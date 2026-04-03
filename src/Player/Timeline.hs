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
  , compileSectionNotes
  , prepareTimelineRuntime
  , sectionBarFrames
  , setTimelineTargets
  , popReadyBars
  , popRetargetTelemetry
  , popLookaheadTelemetry
  , popTransitionTelemetry
  , stampLookaheadPerfMetrics
  , setTimelineGenre
  , validateSongGenre
  , generatedInstrumentSpecs
  , lookupGeneratedInstrument
  , timelineNextBarBoundaryFrame
  , timelineBoundarySpanFromNextBar
  , timelineRuntimeDone
  , timelineRuntimeEndFrame
  ) where

import Player.Timeline.Arrangement (compileSectionNotes, compileTimelineBars, cueSectionOrder, sectionBarFrames)
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
  , stampLookaheadPerfMetrics
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
