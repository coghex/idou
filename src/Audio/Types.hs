{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Types
  ( AudioMsg(..)
  , AudioHandle(..)
  , NoteId(..)
  , Waveform(..)
  , InstrumentId(..)
  , Instrument(..)
  , PitchSpec(..)
  , SyncSpec(..)
  , OscLayer(..)
  ) where

import Data.Word (Word32)
import Engine.Core.Queue (Queue)
import Audio.Envelope (ADSR)
import Audio.Filter.Types (FilterSpec)

data NoteId
  = NoteMidi !Int
  | NoteHz   !Float
  deriving (Eq, Show)

data Waveform
  = WaveSine
  | WaveSaw
  | WaveSquare
  | WaveTriangle
  deriving (Eq, Show)

newtype InstrumentId = InstrumentId Int
  deriving (Eq, Ord, Show)

-- pitch transform relative to the voice base note.
data PitchSpec = PitchSpec
  { psOctaves   ∷ !Int
  , psSemitones ∷ !Float
  , psCents     ∷ !Float
  , psHzOffset  ∷ !Float
  } deriving (Eq, Show)

-- hard sync routing for an oscillator layer.
data SyncSpec
  = NoSync
  | HardSyncTo
      { masterLayerIx ∷ !Int   -- 0..3; must be < slave index
      }
  deriving (Eq, Show)

-- one oscillator "layer" inside an instrument voice.
data OscLayer = OscLayer
  { olWaveform ∷ !Waveform
  , olPitch    ∷ !PitchSpec
  , olLevel    ∷ !Float
  , olSync     ∷ !SyncSpec   -- NEW
  } deriving (Eq, Show)

data Instrument = Instrument
  { -- NEW: up to 4 layers; interpret [] as “default single layer” (WaveSine, unity).
    -- For Step A we’ll enforce non-empty in your code that constructs instruments.
    iOscs        ∷ ![OscLayer]

  , iAdsrDefault ∷ !ADSR
  , iGain        ∷ !Float
  , iFilter      ∷ !(Maybe FilterSpec)
  } deriving (Eq, Show)

data AudioMsg
  = AudioSetInstrument
      { instrumentId ∷ !InstrumentId
      , instrument   ∷ !Instrument
      }

  | AudioSetGlideSec
      { instrumentId ∷ !InstrumentId
      , glideSec     ∷ !Float
      }

  | AudioSetLegatoFilterRetrig
      { instrumentId ∷ !InstrumentId
      , retrigFilter ∷ !Bool
      }

  | AudioSetLegatoAmpRetrig
      { instrumentId ∷ !InstrumentId
      , retrigAmp    ∷ !Bool
      }

  | AudioSetVibrato
      { instrumentId  ∷ !InstrumentId
      , vibRateHz     ∷ !Float
      , vibDepthCents ∷ !Float
      }

  | AudioPlayBeep
      { amp     ∷ !Float
      , pan     ∷ !Float
      , freqHz  ∷ !Float
      , durSec  ∷ !Float
      , adsr    ∷ !ADSR
      }

  | AudioNoteOn
      { instrumentId ∷ !InstrumentId
      , amp          ∷ !Float
      , pan          ∷ !Float
      , noteId       ∷ !NoteId
      , adsrOverride ∷ !(Maybe ADSR)
      }

  | AudioNoteOff
      { instrumentId ∷ !InstrumentId
      , noteId       ∷ !NoteId
      }

  | AudioNoteOffInstrument
      { instrumentId ∷ !InstrumentId
      }

  | AudioStopAll
  | AudioShutdown
  deriving (Eq, Show)

data AudioHandle = AudioHandle
  { audioQueue ∷ Queue AudioMsg
  , sampleRate ∷ !Word32
  , channels   ∷ !Word32
  }
