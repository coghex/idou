{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Oscillator
  ( Osc(..)
  , oscInit
  , oscInitSeeded
  , oscConfigureLayer
  , oscStep
  , oscStepWrap
  , oscResetPhase0
  , hzToPhaseInc
  , oscSetHz
  , oscSetGlideSec
  , oscReleaseAmpEnv
  ) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Word (Word32)

import Audio.Envelope (ADSR, EnvState, envInit, envRelease, envStep)
import Audio.Types (Waveform(..))

data PinkState = PinkState
  { psB0 ∷ !Float
  , psB1 ∷ !Float
  , psB2 ∷ !Float
  , psB3 ∷ !Float
  , psB4 ∷ !Float
  , psB5 ∷ !Float
  , psB6 ∷ !Float
  } deriving (Eq, Show)

data Osc = Osc
  { oWaveform    ∷ !Waveform
  , oAmpEnvSpec  ∷ !(Maybe ADSR)
  , oAmpEnvState ∷ !(Maybe EnvState)
  , oNoiseSeed   ∷ !Word32
  , oPinkState   ∷ !PinkState
  , oSampleRate  ∷ !Float
  , oPhase       ∷ !Float   -- 0..1

  -- current + target phase increment for portamento
  , oPhaseInc    ∷ !Float   -- current cycles/sample
  , oTargetInc   ∷ !Float   -- target cycles/sample

  -- smoothing coefficient in [0,1]; 0 = no change, 1 = jump immediately
  , oGlideCoeff  ∷ !Float

  -- Triangle integrator state (used only for WaveTriangle).
  , oTriState    ∷ !Float
  } deriving (Eq, Show)

defaultNoiseSeed ∷ Word32
defaultNoiseSeed = 0x6D2B79F5

pinkStateInit ∷ PinkState
pinkStateInit = PinkState 0 0 0 0 0 0 0

sanitizeNoiseSeed ∷ Word32 → Word32
sanitizeNoiseSeed x
  | x == 0 = defaultNoiseSeed
  | otherwise = x

oscInit ∷ Waveform → Maybe ADSR → Float → Float → Osc
oscInit wf ampEnv sampleRate hz =
  oscInitSeeded wf ampEnv sampleRate hz defaultNoiseSeed

oscInitSeeded ∷ Waveform → Maybe ADSR → Float → Float → Word32 → Osc
oscInitSeeded wf ampEnv sampleRate hz seed =
  let inc = hzToPhaseInc sampleRate hz
  in Osc
      { oWaveform = wf
      , oAmpEnvSpec = ampEnv
      , oAmpEnvState = fmap (const envInit) ampEnv
      , oNoiseSeed = sanitizeNoiseSeed seed
      , oPinkState = pinkStateInit
      , oSampleRate = max 1 sampleRate
      , oPhase = 0
      , oPhaseInc = inc
      , oTargetInc = inc
      , oGlideCoeff = 1
      , oTriState = 0
      }

oscConfigureLayer ∷ Waveform → Maybe ADSR → Maybe Word32 → Bool → Osc → Osc
oscConfigureLayer wf ampEnv mSeed retrigAmpEnv o0 =
  let envState =
        case ampEnv of
          Nothing -> Nothing
          Just _ ->
            if retrigAmpEnv
              then Just envInit
              else case oAmpEnvState o0 of
                     Just st -> Just st
                     Nothing -> Just envInit
      o1 =
        o0
          { oWaveform = wf
          , oAmpEnvSpec = ampEnv
          , oAmpEnvState = envState
          }
  in case mSeed of
       Nothing -> o1
       Just seed ->
         o1
           { oNoiseSeed = sanitizeNoiseSeed seed
           , oPinkState = pinkStateInit
           }

hzToPhaseInc ∷ Float → Float → Float
hzToPhaseInc sampleRate hz =
  if sampleRate <= 0 then 0 else hz / sampleRate

oscSetHz ∷ Float → Float → Osc → Osc
oscSetHz sampleRate hz o =
  o { oTargetInc = hzToPhaseInc sampleRate hz }

oscSetGlideSec ∷ Float → Float → Osc → Osc
oscSetGlideSec sampleRate glideSec o =
  let coeff
        | glideSec <= 0 = 1
        | sampleRate <= 0 = 1
        | otherwise =
            let a = exp (-(1 / (glideSec * sampleRate)))
            in 1 - realToFrac a
  in o { oGlideCoeff = coeff }

-- NEW
oscResetPhase0 ∷ Osc → Osc
oscResetPhase0 o = o { oPhase = 0 }

oscReleaseAmpEnv ∷ Osc → Osc
oscReleaseAmpEnv o = o { oAmpEnvState = envRelease <$> oAmpEnvState o }

-- | Step oscillator by one sample, returning (newOsc, sample in [-1,1]).
oscStep ∷ Osc → (Osc, Float)
oscStep o0 =
  let (o1, s, _wrapped) = oscStepWrap o0
  in (o1, s)

-- NEW: like oscStep, but also returns phase wrap indicator.
oscStepWrap ∷ Osc → (Osc, Float, Bool)
oscStepWrap o0 =
  let coeff = oGlideCoeff o0
      inc0  = oPhaseInc o0
      incT  = oTargetInc o0
      inc1  = inc0 + coeff * (incT - inc0)

      ph0 = oPhase o0
      (rawSample, tri1, pink1, seed1) =
        oscSample (oWaveform o0) ph0 inc1 (oTriState o0) (oPinkState o0) (oNoiseSeed o0)
      (envState1, envMul) =
        case oAmpEnvSpec o0 of
          Nothing -> (Nothing, 1)
          Just adsr ->
            let env0 =
                  case oAmpEnvState o0 of
                    Just st -> st
                    Nothing -> envInit
                (env1, envAmp) = envStep (oSampleRate o0) adsr env0
            in (Just env1, envAmp)
      s = rawSample * envMul

      p1 = ph0 + inc1
      wrapped = p1 >= 1
      p' = p1 - fromIntegral (floor p1 ∷ Int)
      o1 =
        o0
          { oAmpEnvState = envState1
          , oNoiseSeed = seed1
          , oPinkState = pink1
          , oPhase = p'
          , oPhaseInc = inc1
          , oTriState = tri1
          }
  in (o1, s, wrapped)

-- PolyBLEP
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

oscSample ∷ Waveform → Float → Float → Float → PinkState → Word32 → (Float, Float, PinkState, Word32)
oscSample wf ph dt tri0 pink0 seed0 =
  case wf of
    WaveSine ->
      (sin (2*pi*ph), tri0, pink0, seed0)

    WaveSaw ->
      let naive = 2*ph - 1
      in (naive - polyBlep ph dt, tri0, pink0, seed0)

    WaveSquare ->
      (blSquare ph dt, tri0, pink0, seed0)

    WaveTriangle ->
      let sq = blSquare ph dt
          k  = 4 * dt
          leak = 0.001
          tri1 = (1 - leak) * (tri0 + k * sq)
          triOut = clamp (-1) 1 tri1
      in (triOut, tri1, pink0, seed0)

    WaveWhiteNoise ->
      let (seed1, white) = nextNoiseSample seed0
      in (white, tri0, pink0, seed1)

    WavePinkNoise ->
      let (seed1, white) = nextNoiseSample seed0
          (pink1, pinkOut) = pinkSample pink0 white
      in (pinkOut, tri0, pink1, seed1)

    WaveNoiseMix blend ->
      let (seed1, white) = nextNoiseSample seed0
          (pink1, pinkOut) = pinkSample pink0 white
          amt = clamp 0 1 blend
      in (((1 - amt) * white) + (amt * pinkOut), tri0, pink1, seed1)

nextNoiseSample ∷ Word32 → (Word32, Float)
nextNoiseSample seed0 =
  let x1 = seed0 `xor` (seed0 `shiftL` 13)
      x2 = x1 `xor` (x1 `shiftR` 17)
      x3 = sanitizeNoiseSeed (x2 `xor` (x2 `shiftL` 5))
      unit ∷ Double
      unit = fromIntegral x3 / fromIntegral (maxBound ∷ Word32)
      sample = realToFrac (unit * 2 - 1)
  in (x3, sample)

pinkSample ∷ PinkState → Float → (PinkState, Float)
pinkSample pink0 white =
  let b0 = 0.99886 * psB0 pink0 + white * 0.0555179
      b1 = 0.99332 * psB1 pink0 + white * 0.0750759
      b2 = 0.96900 * psB2 pink0 + white * 0.1538520
      b3 = 0.86650 * psB3 pink0 + white * 0.3104856
      b4 = 0.55000 * psB4 pink0 + white * 0.5329522
      b5 = (-0.7616) * psB5 pink0 - white * 0.0168980
      b6 = white * 0.115926
      pink1 = PinkState b0 b1 b2 b3 b4 b5 b6
      sample = clamp (-1) 1 ((b0 + b1 + b2 + b3 + b4 + b5 + psB6 pink0 + white * 0.5362) * 0.11)
  in (pink1, sample)
