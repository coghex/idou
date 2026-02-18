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
import Foreign (Ptr)
import Foreign.C (CFloat)
import Foreign.ForeignPtr (ForeignPtr)

import qualified Data.Vector.Mutable as MV

import Audio.Types (InstrumentId, Instrument, NoteKey, NoteInstanceId)
import Audio.Envelope (ADSR, EnvState)
import Audio.Oscillator (Osc)
import Audio.Filter (FilterState)
import Sound.Miniaudio.RingBuffer (MaRB)

sr ∷ Word32
sr = 48000

maxLayers ∷ Int
maxLayers = 4

data Voice = Voice
  { vOscs       ∷ !(MV.IOVector Osc)
  , vOscCount   ∷ !Int
  , vLevels     ∷ !(MV.IOVector Float)
  , vPitchRat   ∷ !(MV.IOVector Float)
  , vHzOffset   ∷ !(MV.IOVector Float)
  , vSyncMaster ∷ !(MV.IOVector Int)
  , vLayerGainL ∷ !(MV.IOVector Float)
  , vLayerGainR ∷ !(MV.IOVector Float)

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

  -- NEW: MIDI-native identity + velocity
  , vNoteKey        ∷ !(Maybe NoteKey)
  , vNoteInstanceId ∷ !(Maybe NoteInstanceId)
  , vVelocity       ∷ !Float

  , vInstrId    ∷ !(Maybe InstrumentId)

  , vStartedAt  ∷ !Word64
  , vVibPhase   ∷ !Float
  , vLfo1Phase  ∷ !Float
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
