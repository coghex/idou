{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Filter processing facade.
--
-- Current scope:
--   - Implements only cascaded biquads for a single 'FilterSpec' (slope stages).
--   - Designed so you can later extend to 'FilterProfile' graphs (series/parallel).
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
  ( BiquadCoeffs, BiquadState, biquadCoeffs, biquadStateInit, biquadStep )

--------------------------------------------------------------------------------
-- State (per voice)
--------------------------------------------------------------------------------

-- | Per-voice filter state for a cascaded biquad filter.
-- We store:
--   - coefficients per stage (all stages identical for now)
--   - biquad state per stage
--
-- This is intentionally simple for v1; later you can:
--   - compute different coeffs per stage (for more accurate slopes)
--   - add parallel routing
data FilterState = FilterState
  { fsSpec   ∷ !FilterSpec
  , fsCoeffs ∷ ![BiquadCoeffs]
  , fsStates ∷ ![BiquadState]
  } deriving (Eq, Show)

stagesFor ∷ FilterSlope → Int
stagesFor s =
  case s of
    S12 -> 1
    S24 -> 2
    S36 -> 3
    S48 -> 4

filterStateInit
  ∷ Float       -- ^ sampleRate
  → Float       -- ^ noteHz (for key tracking)
  → FilterSpec
  → FilterState
filterStateInit sampleRate noteHz spec =
  let cutoff = keyTrackedCutoff noteHz spec
      stages = stagesFor (fSlope spec)
      c      = biquadCoeffs (fType spec) sampleRate cutoff (fQ spec)
  in FilterState
      { fsSpec   = spec
      , fsCoeffs = replicate stages c
      , fsStates = replicate stages biquadStateInit
      }

--------------------------------------------------------------------------------
-- Key tracking
--------------------------------------------------------------------------------

-- | Apply key tracking to the base cutoff.
--
-- We treat tracking as:
--   cutoff = baseCutoff * (noteHz/440)^keyTrack
--
-- This has a nice property:
--   - keyTrack=0 => cutoff = baseCutoff
--   - keyTrack=1 => cutoff scales proportionally with pitch
--
-- You can later change the reference (440) or use MIDI note math; this is continuous in Hz.
keyTrackedCutoff ∷ Float → FilterSpec → Float
keyTrackedCutoff noteHz spec =
  let KeyTrack kt = fKeyTrack spec
      base = fCutoffHz spec
      ratio = if noteHz <= 0 then 1 else noteHz / 440
  in base * (ratio ** kt)

--------------------------------------------------------------------------------
-- Processing
--------------------------------------------------------------------------------

-- | Process one sample through the filter chain.
filterStep ∷ Float → FilterState → Float → (FilterState, Float)
filterStep _sampleRate fs0 x0 =
  let go [] [] x = ([], [], x)
      go (c:cs) (s:ss) x =
        let (s', y) = biquadStep c s x
            (cs', ss', z) = go cs ss y
        in (c:cs', s':ss', z)
      go _ _ x = (fsCoeffs fs0, fsStates fs0, x) -- should not happen
      (cs', ss', y) = go (fsCoeffs fs0) (fsStates fs0) x0
  in (fs0 { fsStates = ss' }, y)
