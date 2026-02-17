{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Thread.Types
  ( Voice(..)
  , AudioState(..)
  , AudioUserData(..)
  , sr
  , clamp01
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

data Voice = Voice
  { vOsc        ∷ !Osc
  , vFilter     ∷ !(Maybe FilterState)
  , vFiltEnv    ∷ !(Maybe EnvState)
  , vFiltTick   ∷ !Int
  , vNoteHz     ∷ !Float
  , vBaseInc    ∷ !Float
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
  , stMixBuf         ∷ !(ForeignPtr CFloat) -- interleaved stereo
  , stInstruments    ∷ !(MV.IOVector (Maybe Instrument)) -- length 256
  , stGlideSec       ∷ !(MV.IOVector Float) -- length 256
  , stLegFiltRetrig  ∷ !(MV.IOVector Bool)  -- length 256
  , stLegAmpRetrig   ∷ !(MV.IOVector Bool)  -- length 256
  , stVibRateHz      ∷ !(MV.IOVector Float) -- length 256
  , stVibDepthCents  ∷ !(MV.IOVector Float) -- length 256
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
