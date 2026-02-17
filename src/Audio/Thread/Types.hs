{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Thread.Types
  ( Voice(..)
  , AudioState(..)
  , AudioUserData(..)
  , sr
  , clamp01
  , maxLayers
  ) where

import Data.IORef (IORef)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.C (CFloat)
import Foreign.ForeignPtr (ForeignPtr)

import qualified Data.Vector.Mutable as MV

import Audio.Types (NoteId, InstrumentId, Instrument)
import Audio.Envelope (ADSR, EnvState)
import Audio.Oscillator (Osc)
import Audio.Filter (FilterState)
import Sound.Miniaudio.RingBuffer (MaRB)

sr ∷ Word32
sr = 48000

maxLayers ∷ Int
maxLayers = 4

data Voice = Voice
  { -- NEW: up to 4 osc layers inline (no lists/allocations).
    vOscs       ∷ !(MV.IOVector Osc)     -- length maxLayers, always allocated per voice
  , vOscCount   ∷ !Int                   -- number of active layers (1..maxLayers)
  , vLevels     ∷ !(MV.IOVector Float)   -- per-layer level (0..1), length maxLayers
  , vPitchRat   ∷ !(MV.IOVector Float)   -- per-layer multiplicative ratio vs baseHz, length maxLayers
  , vHzOffset   ∷ !(MV.IOVector Float)   -- per-layer additive Hz offset, length maxLayers
  , vSyncMaster ∷ !(MV.IOVector Int)
  , vFilter     ∷ !(Maybe FilterState)
  , vFiltEnv    ∷ !(Maybe EnvState)
  , vFiltTick   ∷ !Int

  , vNoteHz     ∷ !Float
  , vHoldRemain ∷ !Int

  , vBaseAmpL   ∷ !Float
  , vBaseAmpR   ∷ !Float
  , vAmpL       ∷ !Float
  , vAmpR       ∷ !Float

  , vADSR       ∷ !ADSR
  , vEnv        ∷ !EnvState
  , vNoteId     ∷ !(Maybe NoteId)
  , vInstrId    ∷ !(Maybe InstrumentId)

  , vStartedAt  ∷ !Word64
  , vVibPhase   ∷ !Float
  }

data AudioState = AudioState
  { stVoices         ∷ !(MV.IOVector Voice)
  , stActiveCount    ∷ !Int
  , stMixBuf         ∷ !(ForeignPtr CFloat)
  , stInstruments    ∷ !(MV.IOVector (Maybe Instrument))
  , stGlideSec       ∷ !(MV.IOVector Float)
  , stLegFiltRetrig  ∷ !(MV.IOVector Bool)
  , stLegAmpRetrig   ∷ !(MV.IOVector Bool)
  , stVibRateHz      ∷ !(MV.IOVector Float)
  , stVibDepthCents  ∷ !(MV.IOVector Float)
  , stNow            ∷ !Word64
  }

data AudioUserData = AudioUserData
  { aud_rb        ∷ !(Ptr MaRB)
  , aud_channels  ∷ !Word32
  , aud_underruns ∷ !(IORef Word64)
  }

clamp01 ∷ Float → Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
