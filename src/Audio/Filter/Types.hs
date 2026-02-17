{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Filter.Types
  ( FilterType(..)
  , FilterSlope(..)
  , slopeStages
  , KeyTrack(..)
  , FilterSpec(..)
  , FilterProfile(..)
  ) where

import Audio.Envelope (ADSR)
import Audio.Filter.Biquad (FilterType(..))

data FilterSlope
  = S12
  | S24
  | S36
  | S48
  deriving (Eq, Show)

slopeStages ∷ FilterSlope → Int
slopeStages s =
  case s of
    S12 -> 1
    S24 -> 2
    S36 -> 3
    S48 -> 4

-- | Key tracking amount, where 0 = none, 1 = 1 octave cutoff per octave note.
newtype KeyTrack = KeyTrack Float
  deriving (Eq, Show)

data FilterSpec = FilterSpec
  { fType         ∷ !FilterType
  , fCutoffHz     ∷ !Float
  , fQ            ∷ !Float
  , fSlope        ∷ !FilterSlope
  , fKeyTrack     ∷ !KeyTrack

  -- Envelope -> cutoff in octaves:
  , fEnvAmountOct ∷ !Float
  , fEnvADSR      ∷ !ADSR
  -- Envelope -> Q (additive). Effective Q = baseQ + fQEnvAmount * envLevel
  -- You can set this to 0 for "no Q modulation".
  , fQEnvAmount   ∷ !Float
  } deriving (Eq, Show)

data FilterProfile
  = FPNode !FilterSpec
  | FPSeries ![FilterProfile]
  | FPParallel ![FilterProfile]
  deriving (Eq, Show)
