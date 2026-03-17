{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Timeline.Types
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
  ) where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Word (Word32, Word64)

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

data ChordRegionSpec = ChordRegionSpec
  { crStartBeat ∷ !Float
  , crChord     ∷ !ChordSpec
  }
  deriving (Eq, Show)

data InstrumentPatternSpec = InstrumentPatternSpec
  { ipName        ∷ !String
  , ipInstrumentId ∷ !InstrumentId
  , ipPatchPath   ∷ !(Maybe FilePath)
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
  , ssChordRegions  ∷ ![ChordRegionSpec]
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

data TimelineRetargetTelemetry = TimelineRetargetTelemetry
  { trtReason               ∷ !String
  , trtPolicy               ∷ !String
  , trtBufferedFutureBars   ∷ !Int
  , trtNextGeneratedBarIx   ∷ !Int
  , trtNextGeneratedFrame   ∷ !Word64
  , trtMoodTarget           ∷ !(Maybe String)
  , trtEnergyTarget         ∷ !Float
  , trtGenre                ∷ !String
  }
  deriving (Eq, Show)

data TimelineLookaheadTelemetry = TimelineLookaheadTelemetry
  { tltReason             ∷ !String
  , tltGeneratedBarCount  ∷ !Int
  , tltFirstGeneratedBarIx ∷ !Int
  , tltLastGeneratedBarIx ∷ !Int
  , tltStartFrame         ∷ !Word64
  , tltEndFrame           ∷ !Word64
  , tltBufferedFutureBars ∷ !Int
  , tltMoodTarget         ∷ !(Maybe String)
  , tltEnergyTarget       ∷ !Float
  , tltGenre              ∷ !String
  , tltSeedStart          ∷ !Word64
  , tltSeedEnd            ∷ !Word64
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
  { trBars               ∷ !(Seq TimelineBar)
  , trStartFrame         ∷ !Word64
  , trLookaheadBars      ∷ !Int
  , trNextBarIx          ∷ !Int
  , trBarOffset          ∷ !Int
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
  , trPendingRetargets   ∷ ![TimelineRetargetTelemetry]
  , trPendingLookahead   ∷ ![TimelineLookaheadTelemetry]
  , trPendingTransitions ∷ ![TimelineTransitionTelemetry]
  , trDone               ∷ !Bool
  }
  deriving (Eq, Show)
