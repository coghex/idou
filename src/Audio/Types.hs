{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Types
  ( AudioMsg(..)
  , AudioHandle(..)
  , NoteId(..)
  , Waveform(..)
  , InstrumentId(..)
  , Instrument(..)
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

data Instrument = Instrument
  { iWaveform    ∷ !Waveform
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

  -- NEW: per-instrument behavior on legato note changes:
  -- if True, retrigger the AMP envelope; if False, keep it running (true legato).
  | AudioSetLegatoAmpRetrig
      { instrumentId ∷ !InstrumentId
      , retrigAmp    ∷ !Bool
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
