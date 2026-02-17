{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Oscillator
  ( Osc(..)
  , oscInit
  , oscStep
  , hzToPhaseInc
  , oscSetHz
  , oscSetGlideSec
  ) where

import Audio.Types (Waveform(..))

data Osc = Osc
  { oWaveform    ∷ !Waveform
  , oPhase       ∷ !Float   -- 0..1

  -- current + target phase increment for portamento
  , oPhaseInc    ∷ !Float   -- current cycles/sample
  , oTargetInc   ∷ !Float   -- target cycles/sample

  -- smoothing coefficient in [0,1]; 0 = no change, 1 = jump immediately
  , oGlideCoeff  ∷ !Float
  } deriving (Eq, Show)

oscInit ∷ Waveform → Float → Float → Osc
oscInit wf sampleRate hz =
  let inc = hzToPhaseInc sampleRate hz
  in Osc
      { oWaveform = wf
      , oPhase = 0
      , oPhaseInc = inc
      , oTargetInc = inc
      , oGlideCoeff = 1  -- default: no glide unless configured
      }

hzToPhaseInc ∷ Float → Float → Float
hzToPhaseInc sampleRate hz =
  if sampleRate <= 0 then 0 else hz / sampleRate

-- | Set target frequency (for legato glide).
oscSetHz ∷ Float → Float → Osc → Osc
oscSetHz sampleRate hz o =
  o { oTargetInc = hzToPhaseInc sampleRate hz }

-- | Configure glide time (seconds). 0 disables glide.
-- We compute a per-sample smoothing coefficient:
--   inc += coeff * (target - inc)
-- with:
--   coeff = 1 - exp(-1/(glideSec*sampleRate))
oscSetGlideSec ∷ Float → Float → Osc → Osc
oscSetGlideSec sampleRate glideSec o =
  let coeff
        | glideSec <= 0 = 1
        | sampleRate <= 0 = 1
        | otherwise =
            let a = exp (-(1 / (glideSec * sampleRate)))
            in 1 - realToFrac a
  in o { oGlideCoeff = coeff }

-- | Step oscillator by one sample, returning (newOsc, sample in [-1,1]).
oscStep ∷ Osc → (Osc, Float)
oscStep o0 =
  let coeff = oGlideCoeff o0
      inc0  = oPhaseInc o0
      incT  = oTargetInc o0
      inc1  = inc0 + coeff * (incT - inc0)

      ph0 = oPhase o0
      s   = oscSample (oWaveform o0) ph0 inc1

      p1 = ph0 + inc1
      p' = p1 - fromIntegral (floor p1 ∷ Int)
      o1 = o0 { oPhase = p', oPhaseInc = inc1 }
  in (o1, s)

-- PolyBLEP as you already have
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
      sin (2*pi*ph)

    WaveSaw ->
      let naive = 2*ph - 1
      in naive - polyBlep ph dt

    WaveSquare ->
      let naive = if ph < 0.5 then 1 else -1
          blep0  = polyBlep ph dt
          ph2    = let x = ph + 0.5 in if x >= 1 then x - 1 else x
          blep05 = polyBlep ph2 dt
      in naive + blep0 - blep05

    WaveTriangle ->
      1 - 4 * abs (ph - 0.5)
