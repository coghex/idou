{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Filter
  ( FilterType(..)
  , FilterSlope(..)
  , KeyTrack(..)
  , FilterSpec(..)
  , FilterProfile(..)
  , FilterState(..)
  , filterStateInit
  , keyTrackedBaseCutoff
  , filterEnvCutoff
  , filterEnvQ
  , filterRetune
  , filterStep
  ) where

import Audio.Filter.Types
import Audio.Filter.Biquad
  ( BiquadCoeffs
  , BiquadState
  , biquadCoeffs
  , biquadStateInit
  , biquadStep
  , clampQ
  )

--------------------------------------------------------------------------------
-- State (per voice)
--------------------------------------------------------------------------------

data FilterState = FilterState
  { fsSpec   ∷ !FilterSpec
  , fsStages ∷ !Int            -- 1..4
  , fsCoeffs ∷ !BiquadCoeffs

  , fsS1 ∷ !BiquadState
  , fsS2 ∷ !BiquadState
  , fsS3 ∷ !BiquadState
  , fsS4 ∷ !BiquadState
  } deriving (Eq, Show)

clampStages ∷ Int → Int
clampStages n
  | n < 1 = 1
  | n > 4 = 4
  | otherwise = n

filterStateInit
  ∷ Float       -- ^ sampleRate
  → Float       -- ^ noteHz (for key tracking)
  → FilterSpec
  → FilterState
filterStateInit sampleRate noteHz spec =
  let baseCutoff = keyTrackedBaseCutoff noteHz spec
      nStages    = clampStages (slopeStages (fSlope spec))
      c          = biquadCoeffs (fType spec) sampleRate baseCutoff (fQ spec)
  in FilterState
      { fsSpec = spec
      , fsStages = nStages
      , fsCoeffs = c
      , fsS1 = biquadStateInit
      , fsS2 = biquadStateInit
      , fsS3 = biquadStateInit
      , fsS4 = biquadStateInit
      }

--------------------------------------------------------------------------------
-- Key tracking + envelope mapping helpers
--------------------------------------------------------------------------------

-- | Base cutoff after key tracking, before envelope modulation.
-- cutoff = baseCutoff * (noteHz/440)^keyTrack
keyTrackedBaseCutoff ∷ Float → FilterSpec → Float
keyTrackedBaseCutoff noteHz spec =
  let KeyTrack kt = fKeyTrack spec
      base = fCutoffHz spec
      ratio = if noteHz <= 0 then 1 else noteHz / 440
  in base * (ratio ** kt)

-- | Envelope-modulated cutoff in octaves:
-- cutoff = baseCutoff * 2^(envAmountOct * envLevel)
filterEnvCutoff ∷ Float → Float → FilterSpec → Float
filterEnvCutoff baseCutoff envLevel spec =
  let amt = fEnvAmountOct spec
  in if amt == 0
       then baseCutoff
       else baseCutoff * (2 ** (amt * envLevel))

-- | Envelope-modulated Q (additive):
-- Q = clampQ (baseQ + qAmt * envLevel)
filterEnvQ ∷ Float → FilterSpec → Float
filterEnvQ envLevel spec =
  clampQ (fQ spec + fQEnvAmount spec * envLevel)

--------------------------------------------------------------------------------
-- Retuning + processing
--------------------------------------------------------------------------------

-- | Update coefficients in-place for new cutoff/Q, keeping state (z1/z2) untouched.
filterRetune ∷ Float → Float → Float → FilterState → FilterState
filterRetune sampleRate cutoffHz q fs =
  let spec = fsSpec fs
      c = biquadCoeffs (fType spec) sampleRate cutoffHz q
  in fs { fsCoeffs = c }

filterStep ∷ Float → FilterState → Float → (FilterState, Float)
filterStep _sampleRate fs0 x0 =
  let c = fsCoeffs fs0
      n = fsStages fs0

      (s1', y1) = biquadStep c (fsS1 fs0) x0
      (s2', y2) =
        if n >= 2
          then biquadStep c (fsS2 fs0) y1
          else (fsS2 fs0, y1)
      (s3', y3) =
        if n >= 3
          then biquadStep c (fsS3 fs0) y2
          else (fsS3 fs0, y2)
      (s4', y4) =
        if n >= 4
          then biquadStep c (fsS4 fs0) y3
          else (fsS4 fs0, y3)

      fs1 = fs0 { fsS1 = s1', fsS2 = s2', fsS3 = s3', fsS4 = s4' }
  in (fs1, y4)
