{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Filter.Types
  ( FilterType(..)
  , FilterSlope(..)
  , slopeStages
  , KeyTrack(..)
  , FilterSpec(..)
  , FilterProfile(..)
  ) where

import Audio.Filter.Biquad (FilterType(..))

-- | How many 2-pole stages to cascade (12dB/oct per stage).
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
  { fType      ∷ !FilterType
  , fCutoffHz  ∷ !Float
  , fQ         ∷ !Float
  , fSlope     ∷ !FilterSlope
  , fKeyTrack  ∷ !KeyTrack
  } deriving (Eq, Show)

-- | Future-proof profile type; you can ignore for now.
data FilterProfile
  = FPNode !FilterSpec
  | FPSeries ![FilterProfile]
  | FPParallel ![FilterProfile]
  deriving (Eq, Show)
