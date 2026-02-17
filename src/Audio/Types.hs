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

  -- NEW: release all currently active voices for an instrument,
  -- without needing to know the current NoteId (useful for mono legato).
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
