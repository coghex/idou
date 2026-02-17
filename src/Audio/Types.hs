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

  -- mod matrix
  , ModSrc(..)
  , ModDst(..)
  , ModRoute(..)

  -- NEW: poly / play mode
  , PlayMode(..)
  , VoiceSteal(..)
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
  | HardSyncTo { masterLayerIx ∷ !Int }
  deriving (Eq, Show)

data OscLayer = OscLayer
  { olWaveform ∷ !Waveform
  , olPitch    ∷ !PitchSpec
  , olLevel    ∷ !Float
  , olSync     ∷ !SyncSpec
  } deriving (Eq, Show)

-- Mod matrix
data ModSrc
  = ModSrcLfo1
  | ModSrcEnvAmp
  | ModSrcEnvFilter
  | ModSrcKeyTrack
  deriving (Eq, Show)

data ModDst
  = ModDstLayerPitchCents !Int
  | ModDstFilterCutoffOct
  | ModDstAmpGain
  deriving (Eq, Show)

data ModRoute = ModRoute
  { mrSrc    ∷ !ModSrc
  , mrDst    ∷ !ModDst
  , mrAmount ∷ !Float
  } deriving (Eq, Show)

-- NEW: instrument play mode
data PlayMode
  = MonoLegato
  | Poly
  deriving (Eq, Show)

-- NEW: voice stealing policy
data VoiceSteal
  = StealQuietest
  deriving (Eq, Show)

data Instrument = Instrument
  { iOscs        ∷ ![OscLayer]
  , iLayerSpread ∷ !Float
  , iAdsrDefault ∷ !ADSR
  , iGain        ∷ !Float
  , iFilter      ∷ !(Maybe FilterSpec)
  , iModRoutes   ∷ ![ModRoute]

  -- NEW: poly config
  , iPlayMode    ∷ !PlayMode
  , iPolyMax     ∷ !Int          -- e.g. 8
  , iVoiceSteal  ∷ !VoiceSteal   -- StealQuietest
  } deriving (Eq, Show)

data AudioMsg
  = AudioSetInstrument { instrumentId ∷ !InstrumentId, instrument ∷ !Instrument }
  | AudioSetGlideSec { instrumentId ∷ !InstrumentId, glideSec ∷ !Float }
  | AudioSetLegatoFilterRetrig { instrumentId ∷ !InstrumentId, retrigFilter ∷ !Bool }
  | AudioSetLegatoAmpRetrig { instrumentId ∷ !InstrumentId, retrigAmp ∷ !Bool }
  | AudioSetVibrato { instrumentId  ∷ !InstrumentId, vibRateHz ∷ !Float, vibDepthCents ∷ !Float }
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
