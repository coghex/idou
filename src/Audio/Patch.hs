{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Patch
  ( defaultMidiProgram
  , gmPercussionChannel
  , gmChannelInstrument
  , gmProgramInstrument
  ) where

import Audio.Envelope (ADSR(..))
import Audio.Filter.Biquad (FilterType(..))
import Audio.Filter.Types (FilterSlope(..), FilterSpec(..), KeyTrack(..))
import Audio.Types

defaultMidiProgram ∷ Int
defaultMidiProgram = 0

gmPercussionChannel ∷ Int
gmPercussionChannel = 9

gmChannelInstrument ∷ Int → Int → Instrument
gmChannelInstrument ch program
  | ch == gmPercussionChannel = percussionPatch
  | otherwise = gmProgramInstrument program

gmProgramInstrument ∷ Int → Instrument
gmProgramInstrument program =
  let p = clampInt 0 127 program
      family = p `div` 8
      variant = p `mod` 8
  in case family of
       0  -> pianoPatch variant
       1  -> chromaticPercussionPatch variant
       2  -> organPatch variant
       3  -> guitarPatch variant
       4  -> bassPatch variant
       5  -> stringsPatch variant
       6  -> ensemblePatch variant
       7  -> brassPatch variant
       8  -> reedPatch variant
       9  -> pipePatch variant
       10 -> synthLeadPatch variant
       11 -> synthPadPatch variant
       12 -> synthFxPatch variant
       13 -> ethnicPatch variant
       14 -> tunedPercPatch variant
       _  -> soundFxPatch variant

clampInt ∷ Int → Int → Int → Int
clampInt lo hi x = max lo (min hi x)

variant01 ∷ Int → Float
variant01 v = fromIntegral (clampInt 0 7 v) / 7

rootPitch ∷ PitchSpec
rootPitch = PitchSpec 0 0 0 0

octavePitch ∷ Int → PitchSpec
octavePitch oct = PitchSpec oct 0 0 0

detunePitch ∷ Float → PitchSpec
detunePitch cents = PitchSpec 0 0 cents 0

semiPitch ∷ Float → PitchSpec
semiPitch semi = PitchSpec 0 semi 0 0

osc ∷ Waveform → PitchSpec → Float → OscLayer
osc wf ps lvl = OscLayer wf ps lvl NoSync

syncOsc ∷ Waveform → PitchSpec → Float → Int → OscLayer
syncOsc wf ps lvl master = OscLayer wf ps lvl (HardSyncTo master)

lpFilter ∷ Float → Float → Float → Float → ADSR → FilterSpec
lpFilter cutoffHz q keyTrack envAmount envAdsr =
  FilterSpec
    { fType = FLP
    , fCutoffHz = cutoffHz
    , fQ = q
    , fSlope = S24
    , fKeyTrack = KeyTrack keyTrack
    , fEnvAmountOct = envAmount
    , fEnvADSR = envAdsr
    , fQEnvAmount = 0
    }

bpFilter ∷ Float → Float → Float → Float → ADSR → FilterSpec
bpFilter cutoffHz q keyTrack envAmount envAdsr =
  FilterSpec
    { fType = FBP
    , fCutoffHz = cutoffHz
    , fQ = q
    , fSlope = S12
    , fKeyTrack = KeyTrack keyTrack
    , fEnvAmountOct = envAmount
    , fEnvADSR = envAdsr
    , fQEnvAmount = 0
    }

mkPatch
  ∷ [OscLayer]
  → Float
  → ADSR
  → Float
  → Maybe FilterSpec
  → [ModRoute]
  → Int
  → Instrument
mkPatch layers spread adsr gain filt routes polyMax =
  Instrument
    { iOscs = layers
    , iLayerSpread = spread
    , iAdsrDefault = adsr
    , iGain = gain
    , iFilter = filt
    , iModRoutes = routes
    , iPlayMode = Poly
    , iPolyMax = polyMax
    , iVoiceSteal = StealQuietest
    }

lfoPitch ∷ Int → Float → ModRoute
lfoPitch ix cents = ModRoute ModSrcLfo1 (ModDstLayerPitchCents ix) cents

lfoFilter ∷ Float → ModRoute
lfoFilter octs = ModRoute ModSrcLfo1 ModDstFilterCutoffOct octs

lfoAmp ∷ Float → ModRoute
lfoAmp amt = ModRoute ModSrcLfo1 ModDstAmpGain amt

envPitch ∷ Int → Float → ModRoute
envPitch ix cents = ModRoute ModSrcEnvAmp (ModDstLayerPitchCents ix) cents

waveCycle ∷ Int → Waveform
waveCycle n =
  case n `mod` 4 of
    0 -> WaveSaw
    1 -> WaveSquare
    2 -> WaveTriangle
    _ -> WaveSine

pianoPatch ∷ Int → Instrument
pianoPatch variant =
  let v = variant01 variant
      filt = lpFilter (1800 + 2200 * v) 0.8 0.85 0.8 (ADSR 0.001 0.12 0 0.10)
  in mkPatch
       [ osc WaveSaw rootPitch 0.72
       , osc (if even variant then WaveTriangle else WaveSquare) (detunePitch (3 + 6 * v)) 0.28
       ]
       0.12
       (ADSR 0.002 0.16 (0.16 + 0.10 * v) (0.25 + 0.18 * v))
       (0.90 + 0.08 * v)
       (Just filt)
       [envPitch 0 (-6 - 4 * v)]
       10

chromaticPercussionPatch ∷ Int → Instrument
chromaticPercussionPatch variant =
  let v = variant01 variant
      upperPitch = semiPitch (12 + 5 * v)
      filt = bpFilter (700 + 1400 * v) 1.2 0.3 0.5 (ADSR 0.001 0.04 0 0.05)
  in mkPatch
       [ osc (waveCycle (variant + 3)) rootPitch 0.80
       , osc WaveSine upperPitch 0.20
       ]
       0.04
       (ADSR 0.001 (0.08 + 0.05 * v) 0 (0.10 + 0.05 * v))
       (0.92 + 0.06 * v)
       (Just filt)
       [envPitch 0 (-24 - 8 * v)]
       8

organPatch ∷ Int → Instrument
organPatch variant =
  let v = variant01 variant
  in mkPatch
       [ osc WaveSquare rootPitch 0.55
       , osc WaveSquare (semiPitch 12) 0.25
       , osc WaveTriangle (semiPitch 19) (0.12 + 0.08 * v)
       ]
       (0.15 + 0.10 * v)
       (ADSR 0.006 0.05 1 0.08)
       (0.86 + 0.08 * v)
       Nothing
       [lfoAmp (-0.04), lfoPitch 0 3]
       12

guitarPatch ∷ Int → Instrument
guitarPatch variant =
  let v = variant01 variant
      filt = lpFilter (1200 + 1700 * v) 0.9 0.7 0.4 (ADSR 0.001 0.08 0 0.08)
  in mkPatch
       [ osc WaveSaw rootPitch 0.58
       , osc WaveSquare (detunePitch (2 + 5 * v)) 0.24
       , osc WaveTriangle (semiPitch 12) 0.18
       ]
       0.18
       (ADSR 0.003 (0.18 + 0.05 * v) (0.18 + 0.08 * v) (0.14 + 0.06 * v))
       (0.94 + 0.04 * v)
       (Just filt)
       [envPitch 0 (-10 - 4 * v)]
       8

bassPatch ∷ Int → Instrument
bassPatch variant =
  let v = variant01 variant
      filt = lpFilter (450 + 950 * v) 1.0 0.65 0.5 (ADSR 0.001 0.10 0 0.08)
  in mkPatch
       [ osc WaveSquare (octavePitch (-1)) 0.58
       , osc WaveSaw rootPitch 0.42
       ]
       0.06
       (ADSR 0.002 (0.14 + 0.04 * v) (0.42 + 0.12 * v) (0.14 + 0.05 * v))
       1.04
       (Just filt)
       []
       6

stringsPatch ∷ Int → Instrument
stringsPatch variant =
  let v = variant01 variant
      filt = lpFilter (1900 + 1400 * v) 0.7 0.85 0.2 (ADSR 0.02 0.40 0.5 0.40)
  in mkPatch
       [ osc WaveSaw rootPitch 0.54
       , osc WaveSaw (detunePitch (-5 - 3 * v)) 0.24
       , osc WaveSaw (detunePitch (5 + 3 * v)) 0.22
       ]
       (0.42 + 0.16 * v)
       (ADSR (0.12 + 0.12 * v) 0.45 0.78 (0.75 + 0.30 * v))
       (0.78 + 0.06 * v)
       (Just filt)
       [lfoFilter 0.12, lfoAmp (-0.05)]
       10

ensemblePatch ∷ Int → Instrument
ensemblePatch variant =
  let v = variant01 variant
      filt = lpFilter (1500 + 1200 * v) 0.85 0.75 0.35 (ADSR 0.03 0.30 0.4 0.35)
  in mkPatch
       [ osc WaveSaw rootPitch 0.45
       , osc WaveSquare (detunePitch (-6 - 3 * v)) 0.18
       , osc WaveSaw (detunePitch (6 + 3 * v)) 0.20
       , osc WaveTriangle (semiPitch 12) 0.17
       ]
       (0.50 + 0.14 * v)
       (ADSR (0.08 + 0.10 * v) 0.35 0.82 (0.55 + 0.25 * v))
       (0.80 + 0.06 * v)
       (Just filt)
       [lfoFilter 0.18, lfoAmp (-0.07), lfoPitch 0 4]
       10

brassPatch ∷ Int → Instrument
brassPatch variant =
  let v = variant01 variant
      filt = lpFilter (1400 + 1300 * v) 1.0 0.7 0.7 (ADSR 0.004 0.18 0.1 0.10)
  in mkPatch
       [ osc WaveSaw rootPitch 0.56
       , osc WaveSaw (semiPitch 12) 0.20
       , osc WaveSquare rootPitch 0.18
       ]
       (0.20 + 0.10 * v)
       (ADSR (0.015 + 0.02 * v) 0.20 0.62 (0.18 + 0.08 * v))
       (0.88 + 0.08 * v)
       (Just filt)
       [envPitch 0 (-4), lfoFilter 0.10]
       8

reedPatch ∷ Int → Instrument
reedPatch variant =
  let v = variant01 variant
      filt = bpFilter (900 + 1300 * v) 1.1 0.6 0.25 (ADSR 0.006 0.16 0.2 0.12)
  in mkPatch
       [ osc WaveSquare rootPitch 0.58
       , osc WaveTriangle (detunePitch (2 + 4 * v)) 0.25
       , osc WaveSine (semiPitch 12) 0.17
       ]
       0.16
       (ADSR (0.01 + 0.01 * v) 0.15 0.56 (0.18 + 0.08 * v))
       (0.86 + 0.06 * v)
       (Just filt)
       [lfoPitch 0 7, lfoAmp (-0.04)]
       8

pipePatch ∷ Int → Instrument
pipePatch variant =
  let v = variant01 variant
      filt = lpFilter (2000 + 1600 * v) 0.65 1.0 0.15 (ADSR 0.01 0.18 0.4 0.18)
  in mkPatch
       [ osc WaveSine rootPitch 0.68
       , osc WaveTriangle (semiPitch 12) 0.22
       , osc (if even variant then WaveSquare else WaveSine) (detunePitch (1 + 3 * v)) 0.10
       ]
       0.12
       (ADSR 0.01 0.12 0.82 (0.20 + 0.08 * v))
       (0.82 + 0.08 * v)
       (Just filt)
       [lfoPitch 0 4]
       8

synthLeadPatch ∷ Int → Instrument
synthLeadPatch variant =
  let v = variant01 variant
      filt = lpFilter (1700 + 2200 * v) 0.9 0.75 0.5 (ADSR 0.001 0.12 0 0.08)
  in mkPatch
       [ osc WaveSaw rootPitch 0.66
       , syncOsc WaveSquare (detunePitch (4 + 4 * v)) 0.18 0
       , osc (if even variant then WaveSaw else WaveTriangle) (semiPitch 12) 0.16
       ]
       (0.18 + 0.08 * v)
       (ADSR 0.002 0.10 (0.44 + 0.10 * v) (0.12 + 0.06 * v))
       (0.94 + 0.06 * v)
       (Just filt)
       [lfoPitch 0 18, lfoFilter 0.14]
       6

synthPadPatch ∷ Int → Instrument
synthPadPatch variant =
  let v = variant01 variant
      filt = lpFilter (1100 + 1500 * v) 0.75 0.7 0.3 (ADSR 0.08 0.55 0.6 0.60)
  in mkPatch
       [ osc WaveTriangle rootPitch 0.42
       , osc WaveSaw (detunePitch (-7 - 3 * v)) 0.22
       , osc WaveSaw (detunePitch (7 + 3 * v)) 0.22
       , osc WaveSine (semiPitch 12) 0.14
       ]
       (0.55 + 0.12 * v)
       (ADSR (0.24 + 0.18 * v) 0.70 0.86 (0.90 + 0.40 * v))
       (0.78 + 0.06 * v)
       (Just filt)
       [lfoFilter 0.26, lfoAmp (-0.10)]
       10

synthFxPatch ∷ Int → Instrument
synthFxPatch variant =
  let v = variant01 variant
      filt = bpFilter (800 + 2200 * v) 1.4 0.5 0.7 (ADSR 0.01 0.20 0.15 0.16)
  in mkPatch
       [ osc WaveSaw rootPitch 0.58
       , syncOsc WaveSquare (semiPitch (7 + 5 * v)) 0.22 0
       , osc WaveTriangle (semiPitch 12) 0.20
       ]
       (0.30 + 0.10 * v)
       (ADSR 0.01 (0.20 + 0.08 * v) (0.32 + 0.12 * v) (0.25 + 0.10 * v))
       (0.84 + 0.08 * v)
       (Just filt)
       [lfoPitch 0 15, lfoFilter 0.35, lfoAmp (-0.10)]
       8

ethnicPatch ∷ Int → Instrument
ethnicPatch variant =
  let v = variant01 variant
      filt = bpFilter (1100 + 1600 * v) 1.0 0.65 0.35 (ADSR 0.002 0.12 0.10 0.12)
  in mkPatch
       [ osc (if even variant then WaveTriangle else WaveSquare) rootPitch 0.64
       , osc WaveSine (semiPitch (12 + 2 * v)) 0.16
       , osc WaveSaw (detunePitch (4 + 4 * v)) 0.20
       ]
       (0.16 + 0.10 * v)
       (ADSR 0.004 (0.18 + 0.06 * v) (0.32 + 0.16 * v) (0.18 + 0.08 * v))
       (0.88 + 0.06 * v)
       (Just filt)
       [envPitch 0 (-10), lfoPitch 0 5]
       8

tunedPercPatch ∷ Int → Instrument
tunedPercPatch variant =
  let v = variant01 variant
      filt = bpFilter (1400 + 1600 * v) 1.3 0.35 0.55 (ADSR 0.001 0.05 0 0.06)
  in mkPatch
       [ osc WaveTriangle rootPitch 0.62
       , osc (waveCycle variant) (semiPitch 12) 0.20
       , osc WaveSine (semiPitch 24) 0.10
       ]
       0.08
       (ADSR 0.001 (0.09 + 0.04 * v) 0 (0.10 + 0.05 * v))
       (0.90 + 0.08 * v)
       (Just filt)
       [envPitch 0 (-20 - 10 * v)]
       10

soundFxPatch ∷ Int → Instrument
soundFxPatch variant =
  let v = variant01 variant
      filt = bpFilter (500 + 2500 * v) 1.6 0.2 0.9 (ADSR 0.01 0.25 0.15 0.20)
  in mkPatch
       [ osc (waveCycle (variant + 1)) rootPitch 0.52
       , syncOsc WaveSaw (semiPitch (5 + 10 * v)) 0.24 0
       , osc WaveTriangle (octavePitch 1) 0.16
       ]
       (0.24 + 0.12 * v)
       (ADSR 0.004 (0.18 + 0.12 * v) (0.20 + 0.16 * v) (0.20 + 0.18 * v))
       (0.82 + 0.10 * v)
       (Just filt)
       [lfoPitch 0 24, lfoFilter 0.45, lfoAmp (-0.14)]
       8

percussionPatch ∷ Instrument
percussionPatch =
  let filt = bpFilter 1800 1.3 0.15 1.2 (ADSR 0.001 0.05 0 0.05)
  in mkPatch
       [ osc WaveTriangle rootPitch 0.56
       , osc WaveSquare (octavePitch (-1)) 0.28
       , osc WaveSaw rootPitch 0.16
       ]
       0.04
       (ADSR 0.001 0.08 0 0.09)
       1.0
       (Just filt)
       [envPitch 0 (-36)]
       16
