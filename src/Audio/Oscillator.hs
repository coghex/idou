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

  -- Triangle integrator state (used only for WaveTriangle).
  -- We keep it always so waveform switching can be click-free.
  , oTriState    ∷ !Float
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
      , oTriState = 0
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

      -- Produce sample; triangle needs to update integrator state.
      (s, tri1) = oscSample (oWaveform o0) ph0 inc1 (oTriState o0)

      p1 = ph0 + inc1
      p' = p1 - fromIntegral (floor p1 ∷ Int)
      o1 = o0 { oPhase = p', oPhaseInc = inc1, oTriState = tri1 }
  in (o1, s)

-- | PolyBLEP function for band-limiting waveform discontinuities.
-- t in [0,1), dt = phase increment (cycles/sample).
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

-- | Band-limited square (PolyBLEP corrected).
-- Returns output in roughly [-1,1].
blSquare ∷ Float → Float → Float
blSquare ph dt =
  let naive = if ph < 0.5 then 1 else -1
      blep0  = polyBlep ph dt
      ph2    = let x = ph + 0.5 in if x >= 1 then x - 1 else x
      blep05 = polyBlep ph2 dt
  in naive + blep0 - blep05

clamp ∷ Float → Float → Float → Float
clamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

-- | Sample oscillator. Returns (sample, newTriangleState).
oscSample ∷ Waveform → Float → Float → Float → (Float, Float)
oscSample wf ph dt tri0 =
  case wf of
    WaveSine ->
      (sin (2*pi*ph), tri0)

    WaveSaw ->
      let naive = 2*ph - 1
      in (naive - polyBlep ph dt, tri0)

    WaveSquare ->
      (blSquare ph dt, tri0)

    WaveTriangle ->
      -- Triangle via leaky integration of band-limited square.
      --
      -- Ideal relationship: triangle is integral of square.
      -- Discrete time: tri[n] = tri[n-1] + k * square[n]
      --
      -- Use k = 4*dt so amplitude stays ~[-1,1] across frequency.
      -- Add a tiny leak to avoid DC drift.
      let sq = blSquare ph dt
          k  = 4 * dt
          leak = 0.001  -- small; performance-first, good enough
          tri1 = (1 - leak) * (tri0 + k * sq)
          triOut = clamp (-1) 1 tri1
      in (triOut, tri1)
