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

data PitchSpec = PitchSpec
  { psOctaves   ∷ !Int
  , psSemitones ∷ !Float
  , psCents     ∷ !Float
  , psHzOffset  ∷ !Float
  } deriving (Eq, Show)

data SyncSpec
  = NoSync
  | HardSyncTo
      { masterLayerIx ∷ !Int
      }
  deriving (Eq, Show)

data OscLayer = OscLayer
  { olWaveform ∷ !Waveform
  , olPitch    ∷ !PitchSpec
  , olLevel    ∷ !Float
  , olSync     ∷ !SyncSpec
  } deriving (Eq, Show)

data Instrument = Instrument
  { iOscs        ∷ ![OscLayer]
  , iLayerSpread ∷ !Float          -- NEW: 0..1 stereo spread across layers
  , iAdsrDefault ∷ !ADSR
  , iGain        ∷ !Float
  , iFilter      ∷ !(Maybe FilterSpec)
  } deriving (Eq, Show)

data AudioMsg
  = AudioSetInstrument { instrumentId ∷ !InstrumentId, instrument ∷ !Instrument }
  | AudioSetGlideSec { instrumentId ∷ !InstrumentId, glideSec ∷ !Float }
  | AudioSetLegatoFilterRetrig { instrumentId ∷ !InstrumentId, retrigFilter ∷ !Bool }
  | AudioSetLegatoAmpRetrig { instrumentId ∷ !InstrumentId, retrigAmp ∷ !Bool }
  | AudioSetVibrato { instrumentId ∷ !InstrumentId, vibRateHz ∷ !Float, vibDepthCents ∷ !Float }
  | AudioPlayBeep { amp ∷ !Float, pan ∷ !Float, freqHz ∷ !Float, durSec ∷ !Float, adsr ∷ !ADSR }
  | AudioNoteOn { instrumentId ∷ !InstrumentId, amp ∷ !Float, pan ∷ !Float, noteId ∷ !NoteId, adsrOverride ∷ !(Maybe ADSR) }
  | AudioNoteOff { instrumentId ∷ !InstrumentId, noteId ∷ !NoteId }
  | AudioNoteOffInstrument { instrumentId ∷ !InstrumentId }
  | AudioStopAll
  | AudioShutdown
  deriving (Eq, Show)

data AudioHandle = AudioHandle
  { audioQueue ∷ Queue AudioMsg
  , sampleRate ∷ !Word32
  , channels   ∷ !Word32
  }
