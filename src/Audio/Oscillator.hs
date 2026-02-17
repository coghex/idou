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
  let s = oscSample (oWaveform o0) (oPhase o0)
      p1 = oPhase o0 + oPhaseInc o0
      p' = p1 - fromIntegral (floor p1 ∷ Int)  -- wrap for any increment
      o1 = o0 { oPhase = p' }
  in (o1, s)

oscSample ∷ Waveform → Float → Float
oscSample wf ph =
  case wf of
    WaveSine ->
      -- sin(2*pi*phase)
      let x = 2*pi*ph
      in sin x

    WaveSaw ->
      -- -1..1 ramp
      2*ph - 1

    WaveSquare ->
      if ph < 0.5 then 1 else -1

    WaveTriangle ->
      -- triangle in [-1,1]
      -- 0..1 -> 1..-1..1 shape:
      1 - 4 * abs (ph - 0.5)
