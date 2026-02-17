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
  = ModSrcLfo1                -- bipolar [-1..1]; uses AudioSetVibrato rate
  | ModSrcEnvAmp              -- unipolar [0..1]
  | ModSrcEnvFilter           -- unipolar [0..1] (0 if no filter env)
  | ModSrcKeyTrack            -- unipolar [0..1]
  deriving (Eq, Show)

data ModDst
  = ModDstLayerPitchCents !Int -- 0..3
  | ModDstFilterCutoffOct      -- additive octaves
  | ModDstAmpGain              -- linear delta; final ampMul = max 0 (1 + sum)
  deriving (Eq, Show)

data ModRoute = ModRoute
  { mrSrc    ∷ !ModSrc
  , mrDst    ∷ !ModDst
  , mrAmount ∷ !Float
  } deriving (Eq, Show)

data Instrument = Instrument
  { iOscs        ∷ ![OscLayer]
  , iLayerSpread ∷ !Float
  , iAdsrDefault ∷ !ADSR
  , iGain        ∷ !Float
  , iFilter      ∷ !(Maybe FilterSpec)
  , iModRoutes   ∷ ![ModRoute]      -- NEW
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
      , vibRateHz     ∷ !Float        -- also used as LFO1 rate for ModSrcLfo1
      , vibDepthCents ∷ !Float        -- still used for pitch vibrato (existing feature)
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
