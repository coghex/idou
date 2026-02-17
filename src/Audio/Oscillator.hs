{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Oscillator
  ( Osc(..)
  , oscInit
  , oscStep
  , hzToPhaseInc
  ) where

import Audio.Types (Waveform(..))

data Osc = Osc
  { oWaveform ∷ !Waveform
  , oPhase    ∷ !Float   -- 0..1
  , oPhaseInc ∷ !Float   -- cycles per sample (Hz / sampleRate)
  } deriving (Eq, Show)

oscInit ∷ Waveform → Float → Float → Osc
oscInit wf sampleRate hz =
  Osc
    { oWaveform = wf
    , oPhase = 0
    , oPhaseInc = hzToPhaseInc sampleRate hz
    }

hzToPhaseInc ∷ Float → Float → Float
hzToPhaseInc sampleRate hz =
  if sampleRate <= 0 then 0 else hz / sampleRate

-- | Step oscillator by one sample, returning (newOsc, sample in [-1,1]).
oscStep ∷ Osc → (Osc, Float)
oscStep o0 =
  let dt = oPhaseInc o0
      ph = oPhase o0
      s  = oscSample (oWaveform o0) ph dt
      p1 = ph + dt
      p' = p1 - fromIntegral (floor p1 ∷ Int)  -- wrap for any increment
      o1 = o0 { oPhase = p' }
  in (o1, s)

-- | PolyBLEP function for band-limiting waveform discontinuities.
-- t in [0,1), dt = phase increment (frequency / sampleRate).
polyBlep ∷ Float → Float → Float
polyBlep t dt
  | dt <= 0   = 0
  | t < dt    =
      let x = t / dt
      in x + x - x*x - 1
  | t > 1 - dt =
      let x = (t - 1) / dt
      in x*x + x + x + 1
  | otherwise = 0

oscSample ∷ Waveform → Float → Float → Float
oscSample wf ph dt =
  case wf of
    WaveSine ->
      let x = 2*pi*ph
      in sin x

    WaveSaw ->
      -- Naive saw: 2*ph - 1, discontinuity at phase wrap (ph=0).
      -- PolyBLEP cancels that discontinuity.
      let naive = 2*ph - 1
      in naive - polyBlep ph dt

    WaveSquare ->
      -- Naive square has 2 discontinuities per cycle: at ph=0 and ph=0.5.
      -- We add BLEP at rising edge and subtract at falling edge (or vice versa).
      let naive = if ph < 0.5 then 1 else -1
          -- discontinuity at wrap (ph=0)
          blep0  = polyBlep ph dt
          -- discontinuity at ph=0.5: shift phase by 0.5 and wrap into [0,1)
          ph2    = let x = ph + 0.5 in if x >= 1 then x - 1 else x
          blep05 = polyBlep ph2 dt
      in naive + blep0 - blep05

    WaveTriangle ->
      -- Keep naive triangle for now (already band-limited-ish compared to saw/square).
      -- Next step (optional): integrate band-limited square for a true BLEP triangle.
      1 - 4 * abs (ph - 0.5)
