{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Simple RBJ-cookbook biquad filters (LP/HP/BP) using Transposed Direct Form II.
--   Intended for per-voice filtering in the audio thread.
module Audio.Filter.Biquad
  ( FilterType(..)
  , BiquadCoeffs(..)
  , BiquadState(..)
  , biquadStateInit
  , clampCutoff
  , clampQ
  , biquadCoeffs
  , biquadStep
  ) where

data FilterType
  = FLP  -- ^ lowpass
  | FHP  -- ^ highpass
  | FBP  -- ^ bandpass (constant skirt gain, peak gain = Q)
  deriving (Eq, Show)

-- | Normalized coefficients for:
--     y = b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2
--   but we implement using TDF2 state (z1,z2).
data BiquadCoeffs = BiquadCoeffs
  { b0 ∷ !Float
  , b1 ∷ !Float
  , b2 ∷ !Float
  , a1 ∷ !Float
  , a2 ∷ !Float
  } deriving (Eq, Show)

-- | Transposed Direct Form II state per channel.
data BiquadState = BiquadState
  { z1 ∷ !Float
  , z2 ∷ !Float
  } deriving (Eq, Show)

biquadStateInit ∷ BiquadState
biquadStateInit = BiquadState 0 0

clampCutoff ∷ Float → Float → Float
clampCutoff sampleRate cutoffHz =
  let lo = 5
      hi = 0.49 * sampleRate
  in max lo (min hi cutoffHz)

clampQ ∷ Float → Float
clampQ q =
  let lo = 0.1
      hi = 20.0
  in max lo (min hi q)

-- | Compute RBJ cookbook biquad coefficients for the given type/cutoff/Q.
--   sampleRate must be > 0.
biquadCoeffs ∷ FilterType → Float → Float → Float → BiquadCoeffs
biquadCoeffs ftype sampleRate cutoffHz0 q0 =
  let cutoffHz = clampCutoff sampleRate cutoffHz0
      q        = clampQ q0

      w0  = 2 * pi * cutoffHz / sampleRate
      cw0 = cos w0
      sw0 = sin w0
      alpha = sw0 / (2*q)

      -- Raw (unnormalized) a0 plus others depending on type.
      (b0', b1', b2', a0', a1', a2') =
        case ftype of
          FLP ->
            ( (1 - cw0) / 2
            ,  1 - cw0
            , (1 - cw0) / 2
            ,  1 + alpha
            , -2 * cw0
            ,  1 - alpha
            )

          FHP ->
            ( (1 + cw0) / 2
            , -(1 + cw0)
            , (1 + cw0) / 2
            ,  1 + alpha
            , -2 * cw0
            ,  1 - alpha
            )

          -- RBJ "constant skirt gain, peak gain = Q" bandpass:
          -- b0 =  sin(w0)/2
          -- b1 =  0
          -- b2 = -sin(w0)/2
          -- a0 =  1 + alpha
          -- a1 = -2*cos(w0)
          -- a2 =  1 - alpha
          FBP ->
            (  sw0 / 2
            ,  0
            , -(sw0 / 2)
            ,  1 + alpha
            , -2 * cw0
            ,  1 - alpha
            )

      invA0 = 1 / a0'
  in BiquadCoeffs
      { b0 = b0' * invA0
      , b1 = b1' * invA0
      , b2 = b2' * invA0
      , a1 = a1' * invA0
      , a2 = a2' * invA0
      }

-- | Process one sample through the biquad (TDF2).
--   Returns (newState, y).
biquadStep ∷ BiquadCoeffs → BiquadState → Float → (BiquadState, Float)
biquadStep c s x =
  let y  = b0 c * x + z1 s
      z1' = b1 c * x - a1 c * y + z2 s
      z2' = b2 c * x - a2 c * y
  in (BiquadState z1' z2', y)
