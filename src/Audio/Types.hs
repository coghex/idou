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

data NoteId
  = NoteMidi !Int      -- 0..127 typically
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
  } deriving (Eq, Show)

data AudioMsg
  = AudioSetInstrument
      { instrumentId ∷ !InstrumentId
      , instrument   ∷ !Instrument
      }

  -- Convenience one-shot (kept; does not use instruments)
  | AudioPlayBeep
      { amp     ∷ !Float   -- 0..1
      , pan     ∷ !Float   -- -1..1
      , freqHz  ∷ !Float
      , durSec  ∷ !Float   -- hold time before release
      , adsr    ∷ !ADSR
      }

  -- Held note, uses an instrument.
  | AudioNoteOn
      { instrumentId ∷ !InstrumentId
      , amp          ∷ !Float          -- 0..1
      , pan          ∷ !Float          -- -1..1
      , noteId       ∷ !NoteId
      , adsrOverride ∷ !(Maybe ADSR)   -- Nothing uses instrument default
      }

  | AudioNoteOff
      { instrumentId ∷ !InstrumentId
      , noteId       ∷ !NoteId
      }

  | AudioStopAll
  | AudioShutdown
  deriving (Eq, Show)

data AudioHandle = AudioHandle
  { audioQueue ∷ Queue AudioMsg
  , sampleRate ∷ !Word32
  , channels   ∷ !Word32
  }
