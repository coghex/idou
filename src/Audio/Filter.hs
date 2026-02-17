{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Filter
  ( FilterType(..)
  , FilterSlope(..)
  , KeyTrack(..)
  , FilterSpec(..)
  , FilterProfile(..)
  , FilterState(..)
  , filterStateInit
  , keyTrackedCutoff
  , filterStep
  ) where

import Audio.Filter.Types
import Audio.Filter.Biquad
  ( BiquadCoeffs
  , BiquadState
  , biquadCoeffs
  , biquadStateInit
  , biquadStep
  )

--------------------------------------------------------------------------------
-- State (per voice)
--------------------------------------------------------------------------------

-- | Up to 4 cascaded biquad stages, with a single coefficient set shared by all stages.
-- Stages beyond fsStages are ignored.
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
  let cutoff = keyTrackedCutoff noteHz spec
      nStages = clampStages (slopeStages (fSlope spec))
      c = biquadCoeffs (fType spec) sampleRate cutoff (fQ spec)
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
-- Key tracking
--------------------------------------------------------------------------------

-- | Apply key tracking to the base cutoff.
-- cutoff = baseCutoff * (noteHz/440)^keyTrack
keyTrackedCutoff ∷ Float → FilterSpec → Float
keyTrackedCutoff noteHz spec =
  let KeyTrack kt = fKeyTrack spec
      base = fCutoffHz spec
      ratio = if noteHz <= 0 then 1 else noteHz / 440
  in base * (ratio ** kt)

--------------------------------------------------------------------------------
-- Processing
--------------------------------------------------------------------------------

-- | Process one sample through N cascaded stages.
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
