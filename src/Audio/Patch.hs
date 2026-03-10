{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Patch
  ( defaultMidiProgram
  , gmPercussionChannel
  , gmChannelInstrument
  , gmDrumInstrument
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
  | ch == gmPercussionChannel = gmDrumInstrument 36
  | otherwise = gmProgramInstrument program

gmDrumInstrument ∷ Int → Instrument
gmDrumInstrument key =
  case key of
    35 -> kickPatch 0.92 (-30) 2
    36 -> kickPatch 1.0 (-28) 4
    37 -> rimPatch
    38 -> snarePatch 0.0
    39 -> clapPatch
    40 -> snarePatch 0.2
    41 -> lowFloorTomPatch
    42 -> hatPatch False
    43 -> highFloorTomPatch
    44 -> pedalHatPatch
    45 -> lowTomPatch
    46 -> hatPatch True
    47 -> lowMidTomPatch
    48 -> hiMidTomPatch
    49 -> crashPatch 0.0
    50 -> highTomPatch
    51 -> ridePatch 0.0
    52 -> chinaPatch
    53 -> rideBellPatch
    54 -> tambourinePatch
    55 -> splashPatch
    56 -> cowbellPatch
    57 -> crashPatch 0.7
    58 -> vibraslapPatch
    59 -> ridePatch 0.4
    60 -> bongoPatch 0.0
    61 -> bongoPatch 0.2
    62 -> congaPatch 0.0
    63 -> congaPatch 0.2
    64 -> congaPatch 0.35
    _
      | key < 35 -> kickPatch 0.88 (-31) 1
      | key < 41 -> snarePatch 0.1
      | key < 49 -> tomPatch 0.15
      | key < 57 -> crashPatch 0.25
       | otherwise -> percussionPatch

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
osc wf ps lvl = OscLayer wf ps lvl NoSync Nothing

oscEnv ∷ Waveform → PitchSpec → Float → ADSR → OscLayer
oscEnv wf ps lvl env = OscLayer wf ps lvl NoSync (Just env)

syncOsc ∷ Waveform → PitchSpec → Float → Int → OscLayer
syncOsc wf ps lvl master = OscLayer wf ps lvl (HardSyncTo master) Nothing

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

hpFilter ∷ Float → Float → Float → Float → ADSR → FilterSpec
hpFilter cutoffHz q keyTrack envAmount envAdsr =
  FilterSpec
    { fType = FHP
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

drumPatch
  ∷ [OscLayer]
  → Float
  → ADSR
  → Float
  → Maybe FilterSpec
  → [ModRoute]
  → Instrument
drumPatch layers spread adsr gain filt routes =
  mkPatch layers spread adsr gain filt routes 20

lfoPitch ∷ Int → Float → ModRoute
lfoPitch ix cents = ModRoute ModSrcLfo1 (ModDstLayerPitchCents ix) cents

lfoFilter ∷ Float → ModRoute
lfoFilter octs = ModRoute ModSrcLfo1 ModDstFilterCutoffOct octs

lfoAmp ∷ Float → ModRoute
lfoAmp amt = ModRoute ModSrcLfo1 ModDstAmpGain amt

envPitch ∷ Int → Float → ModRoute
envPitch ix cents = ModRoute ModSrcEnvAmp (ModDstLayerPitchCents ix) cents

aftertouchFilter ∷ ModSrc → Float → ModRoute
aftertouchFilter src octs = ModRoute src ModDstFilterCutoffOct octs

aftertouchAmp ∷ ModSrc → Float → ModRoute
aftertouchAmp src amt = ModRoute src ModDstAmpGain amt

aftertouchPitch ∷ ModSrc → Int → Float → ModRoute
aftertouchPitch src ix cents = ModRoute src (ModDstLayerPitchCents ix) cents

waveCycle ∷ Int → Waveform
waveCycle n =
  case n `mod` 4 of
    0 -> WaveSaw
    1 -> WaveSquare
    2 -> WaveTriangle
    _ -> WaveSine

noiseMix ∷ Float → Waveform
noiseMix amount = WaveNoiseMix (max 0 (min 1 amount))

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
        [ lfoAmp (-0.04)
        , lfoPitch 0 3
        , aftertouchAmp ModSrcChanAftertouch 0.08
        , aftertouchFilter ModSrcChanAftertouch 0.12
        ]
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
        [ lfoFilter 0.12
        , lfoAmp (-0.05)
        , aftertouchFilter ModSrcChanAftertouch 0.24
        , aftertouchAmp ModSrcPolyAftertouch 0.10
        ]
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
        [ lfoFilter 0.18
        , lfoAmp (-0.07)
        , lfoPitch 0 4
        , aftertouchFilter ModSrcChanAftertouch 0.22
        , aftertouchAmp ModSrcPolyAftertouch 0.10
        ]
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
        [ envPitch 0 (-4)
        , lfoFilter 0.10
        , aftertouchFilter ModSrcChanAftertouch 0.18
        , aftertouchAmp ModSrcChanAftertouch 0.12
        ]
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
        [ lfoPitch 0 7
        , lfoAmp (-0.04)
        , aftertouchFilter ModSrcChanAftertouch 0.15
        , aftertouchAmp ModSrcPolyAftertouch 0.08
        ]
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
        [ lfoPitch 0 4
        , aftertouchAmp ModSrcChanAftertouch 0.08
        , aftertouchFilter ModSrcPolyAftertouch 0.12
        ]
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
        [ lfoPitch 0 18
        , lfoFilter 0.14
        , aftertouchFilter ModSrcChanAftertouch 0.30
        , aftertouchPitch ModSrcPolyAftertouch 0 18
        ]
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
        [ lfoFilter 0.26
        , lfoAmp (-0.10)
        , aftertouchFilter ModSrcChanAftertouch 0.32
        , aftertouchAmp ModSrcPolyAftertouch 0.12
        ]
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
        [ lfoPitch 0 15
        , lfoFilter 0.35
        , lfoAmp (-0.10)
        , aftertouchFilter ModSrcChanAftertouch 0.35
        , aftertouchPitch ModSrcPolyAftertouch 1 22
        ]
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
        [ lfoPitch 0 24
        , lfoFilter 0.45
        , lfoAmp (-0.14)
        , aftertouchFilter ModSrcChanAftertouch 0.45
        ]
        8

percussionPatch ∷ Instrument
percussionPatch =
  let filt = bpFilter 1800 1.3 0.15 1.2 (ADSR 0.001 0.05 0 0.05)
  in drumPatch
       [ oscEnv (noiseMix 0.45) rootPitch 0.34 (ADSR 0.0005 0.06 0 0.05)
       , osc WaveTriangle rootPitch 0.42
       , osc WaveSquare (octavePitch (-1)) 0.16
       ]
       0.04
       (ADSR 0.001 0.08 0 0.09)
       1.0
       (Just filt)
       [envPitch 1 (-36)]

kickPatch ∷ Float → Float → Float → Instrument
kickPatch weight bodyTuneSemis attackTuneSemis =
  let clickEnv = ADSR 0.0005 0.03 0 0.04
      filt = lpFilter 150 1.0 0.12 1.0 (ADSR 0.001 0.12 0 0.10)
  in drumPatch
       [ osc WaveSine (semiPitch bodyTuneSemis) 0.82
       , osc WaveTriangle (semiPitch (bodyTuneSemis + 7)) 0.08
       , oscEnv WaveWhiteNoise rootPitch 0.10 clickEnv
       , osc WaveSquare (semiPitch attackTuneSemis) 0.03
       ]
       0
       (ADSR 0.001 0.11 0 0.18)
       (0.62 + 0.10 * weight)
       (Just filt)
       [ envPitch 0 (-56)
       , envPitch 1 (-30)
       , envPitch 3 (-12)
       ]

snarePatch ∷ Float → Instrument
snarePatch variant =
  let wireSnapEnv = ADSR 0.0005 (0.026 + 0.010 * variant) 0 0.020
      wireTailEnv = ADSR 0.0012 (0.070 + 0.018 * variant) 0 0.050
      airEnv = ADSR 0.0010 (0.10 + 0.02 * variant) 0 0.07
      bodyEnv = ADSR 0.0008 (0.11 + 0.02 * variant) 0 0.08
      shellEnv = ADSR 0.0010 (0.14 + 0.02 * variant) 0 0.10
      filt = bpFilter (1320 + 240 * variant) 1.05 0.16 0.34 (ADSR 0.001 0.07 0 0.06)
  in drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.24 wireSnapEnv
       , oscEnv (noiseMix (0.28 + 0.06 * variant)) rootPitch 0.22 wireTailEnv
       , oscEnv WavePinkNoise rootPitch 0.10 airEnv
       , oscEnv WaveTriangle (semiPitch (-18 + 2 * variant)) 0.34 bodyEnv
       , oscEnv WaveSine (semiPitch (-11 + 2 * variant)) 0.22 shellEnv
       , oscEnv WaveTriangle (semiPitch (-6 + 2 * variant)) 0.12 shellEnv
       ]
       0.08
       (ADSR 0.0008 0.15 0 0.11)
       1.06
       (Just filt)
       [ envPitch 3 (-18)
       , envPitch 4 (-10)
       , envPitch 5 (-6)
       ]

clapPatch ∷ Instrument
clapPatch =
  let snapEnv = ADSR 0.0005 0.028 0 0.018
      spreadEnv = ADSR 0.0012 0.060 0 0.040
      airEnv = ADSR 0.0020 0.095 0 0.050
      tailEnv = ADSR 0.0035 0.085 0 0.060
      filt = hpFilter 2600 0.78 0.01 0.18 (ADSR 0.0008 0.03 0 0.04)
  in drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.42 snapEnv
       , oscEnv (noiseMix 0.22) rootPitch 0.30 spreadEnv
       , oscEnv WavePinkNoise rootPitch 0.14 airEnv
       , oscEnv (noiseMix 0.62) rootPitch 0.12 tailEnv
       ]
       0.18
       (ADSR 0.0008 0.09 0 0.07)
       0.84
       (Just filt)
       []

rimPatch ∷ Instrument
rimPatch =
  let filt = bpFilter 2300 1.6 0.1 0.25 (ADSR 0.001 0.03 0 0.04)
  in drumPatch
       [ osc WaveSquare (semiPitch 24) 0.44
       , osc WaveTriangle (semiPitch 36) 0.30
       , osc WaveSine (semiPitch 48) 0.16
       ]
       0.08
       (ADSR 0.001 0.05 0 0.06)
       0.74
       (Just filt)
       [envPitch 0 (-14)]

hatPatch ∷ Bool → Instrument
hatPatch isOpen =
  let decay = if isOpen then 0.34 else 0.14
      release = if isOpen then 0.28 else 0.12
      tickEnv = ADSR 0.0004 (0.020 + if isOpen then 0.010 else 0.004) 0 0.012
      hissEnv = ADSR 0.0005 (decay * 1.10) 0 (release * 0.95)
      washEnv = ADSR 0.0010 (decay * 1.55) 0 (release * 1.20)
      metallicEnv = ADSR 0.0006 (decay * 0.60) 0 (release * 0.70)
      filt = hpFilter (5200 + if isOpen then 200 else 0) 0.78 0.01 0.08 (ADSR 0.001 0.05 0 0.05)
  in drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.24 tickEnv
       , oscEnv (noiseMix 0.84) rootPitch 0.30 hissEnv
       , oscEnv WavePinkNoise rootPitch 0.18 washEnv
       , oscEnv WaveTriangle (semiPitch 64) 0.04 metallicEnv
       ]
       0.08
       (ADSR 0.001 decay 0 release)
       (if isOpen then 0.58 else 0.48)
       (Just filt)
       []

pedalHatPatch ∷ Instrument
pedalHatPatch =
  let noiseEnv = ADSR 0.0005 0.05 0 0.04
      filt = hpFilter 3600 1.0 0.02 0.18 (ADSR 0.001 0.03 0 0.03)
  in drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.30 noiseEnv
       , oscEnv (noiseMix 0.60) rootPitch 0.18 (ADSR 0.0005 0.07 0 0.05)
       , osc WaveSquare (semiPitch 36) 0.18
       , osc WaveTriangle (semiPitch 48) 0.10
       ]
       0.12
       (ADSR 0.001 0.05 0 0.05)
       0.56
       (Just filt)
       []

lowFloorTomPatch, highFloorTomPatch, lowTomPatch, lowMidTomPatch, hiMidTomPatch, highTomPatch ∷ Instrument
lowFloorTomPatch = tomPatch (-0.70)
highFloorTomPatch = tomPatch (-0.42)
lowTomPatch = tomPatch (-0.12)
lowMidTomPatch = tomPatch 0.12
hiMidTomPatch = tomPatch 0.38
highTomPatch = tomPatch 0.64

tomPatch ∷ Float → Instrument
tomPatch position =
  let tone = max 0 (min 1 ((position + 0.70) / 1.34))
      bodySemi = -23 + 12 * position
      subSemi = bodySemi - 11
      ringSemi = bodySemi + 2.3
      knockSemi = bodySemi + 9
      bodyEnv = ADSR 0.0006 (0.12 - 0.02 * tone) 0 0.07
      subEnv = ADSR 0.0008 (0.10 - 0.01 * tone) 0 0.06
      ringEnv = ADSR 0.0007 (0.11 - 0.015 * tone) 0 0.06
      clickEnv = ADSR 0.0004 0.016 0 0.012
      shellEnv = ADSR 0.0006 (0.040 + 0.006 * tone) 0 0.025
      gritEnv = ADSR 0.0008 (0.055 + 0.010 * tone) 0 0.032
      filt = lpFilter (440 + 540 * tone) 0.86 0.24 0.42 (ADSR 0.001 0.08 0 0.05)
  in drumPatch
       [ oscEnv WaveSine (semiPitch bodySemi) 0.42 bodyEnv
       , oscEnv WaveTriangle (semiPitch subSemi) 0.20 subEnv
       , oscEnv WaveTriangle (semiPitch ringSemi) 0.18 ringEnv
       , oscEnv WaveWhiteNoise rootPitch 0.10 clickEnv
       , oscEnv (noiseMix 0.18) rootPitch 0.08 shellEnv
       , oscEnv WavePinkNoise rootPitch 0.05 gritEnv
       , oscEnv WaveSine (semiPitch knockSemi) 0.05 clickEnv
       ]
       0.01
       (ADSR 0.001 0.14 0 0.10)
       0.74
       (Just filt)
       [ envPitch 0 (-46)
       , envPitch 1 (-26)
       , envPitch 2 (-22)
       , envPitch 6 (-12)
       ]

crashPatch ∷ Float → Instrument
crashPatch brightness =
  let strikeEnv = ADSR 0.0008 (0.10 + 0.04 * brightness) 0 0.10
      strikeToneEnv = ADSR 0.0008 (0.12 + 0.05 * brightness) 0 0.12
      whiteEnv = ADSR 0.006 (1.80 + 0.45 * brightness) 0.34 (2.40 + 0.70 * brightness)
      pinkEnv = ADSR 0.010 (2.40 + 0.60 * brightness) 0.42 (3.20 + 0.90 * brightness)
      washEnv = ADSR 0.016 (3.10 + 0.75 * brightness) 0.52 (4.20 + 1.10 * brightness)
      filt = hpFilter (2800 + 320 * brightness) 0.68 0.0 0.08 (ADSR 0.004 0.18 0.24 0.40)
  in
    (drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.22 strikeEnv
       , oscEnv (noiseMix (0.42 + 0.08 * brightness)) rootPitch 0.12 strikeToneEnv
       , oscEnv WaveWhiteNoise rootPitch 0.18 whiteEnv
       , oscEnv WavePinkNoise rootPitch 0.38 pinkEnv
       , oscEnv (noiseMix (0.74 + 0.06 * brightness)) rootPitch 0.28 washEnv
       ]
       0.20
       (ADSR 0.006 (2.40 + 0.60 * brightness) 0.42 (3.60 + 0.90 * brightness))
       0.62
       (Just filt)
       [])
      { iPolyMax = 128
      }

chinaPatch ∷ Instrument
chinaPatch =
  let strikeEnv = ADSR 0.0005 0.08 0 0.06
      trashEnv = ADSR 0.002 0.42 0 0.42
      washEnv = ADSR 0.008 1.30 0.12 1.20
      filt = hpFilter 2400 0.72 0.0 0.10 (ADSR 0.003 0.12 0.14 0.24)
  in
    (drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.22 strikeEnv
       , oscEnv (noiseMix 0.58) rootPitch 0.30 trashEnv
       , oscEnv WavePinkNoise rootPitch 0.22 washEnv
       , oscEnv WaveSaw (semiPitch 23) 0.05 trashEnv
       ]
       0.18
       (ADSR 0.003 0.90 0.12 1.00)
       0.58
       (Just filt)
       [])
      { iPolyMax = 128
      }

ridePatch ∷ Float → Instrument
ridePatch brightness =
  let noiseEnv = ADSR 0.0005 (0.18 + 0.08 * brightness) 0 (0.22 + 0.08 * brightness)
      shimmerEnv = ADSR 0.001 (0.24 + 0.08 * brightness) 0 (0.28 + 0.10 * brightness)
      filt = hpFilter (3000 + 350 * brightness) 0.9 0.02 0.08 (ADSR 0.001 0.03 0 0.06)
  in drumPatch
       [ oscEnv WavePinkNoise rootPitch 0.18 noiseEnv
       , oscEnv (noiseMix (0.45 + 0.20 * brightness)) rootPitch 0.14 shimmerEnv
       , osc WaveSquare (semiPitch 31) 0.22
       , osc WaveSquare (semiPitch 55) 0.18
       ]
       0.22
       (ADSR 0.001 (0.18 + 0.08 * brightness) 0 (0.30 + 0.12 * brightness))
       0.68
       (Just filt)
       []

rideBellPatch ∷ Instrument
rideBellPatch =
  let filt = bpFilter 2600 1.2 0.05 0.20 (ADSR 0.001 0.05 0 0.08)
  in drumPatch
       [ osc WaveSquare (semiPitch 19) 0.38
       , osc WaveSine (semiPitch 31) 0.24
       , osc WaveTriangle (semiPitch 43) 0.18
       ]
       0.10
       (ADSR 0.001 0.14 0 0.14)
       0.66
       (Just filt)
       [envPitch 0 (-9)]

splashPatch ∷ Instrument
splashPatch =
  let strikeEnv = ADSR 0.0005 0.05 0 0.03
      whiteEnv = ADSR 0.002 0.34 0 0.26
      washEnv = ADSR 0.006 0.72 0.08 0.62
      filt = hpFilter 4200 0.74 0.0 0.08 (ADSR 0.002 0.06 0.08 0.14)
  in
    (drumPatch
       [ oscEnv WaveWhiteNoise rootPitch 0.24 strikeEnv
       , oscEnv (noiseMix 0.80) rootPitch 0.22 whiteEnv
       , oscEnv WavePinkNoise rootPitch 0.12 washEnv
       ]
       0.16
       (ADSR 0.002 0.56 0.08 0.48)
       0.54
       (Just filt)
       [])
      { iPolyMax = 128
      }

tambourinePatch ∷ Instrument
tambourinePatch =
  let filt = hpFilter 3600 1.0 0.02 0.16 (ADSR 0.001 0.04 0 0.05)
  in drumPatch
       [ oscEnv (noiseMix 0.40) rootPitch 0.24 (ADSR 0.0005 0.10 0 0.12)
       , oscEnv WaveWhiteNoise rootPitch 0.18 (ADSR 0.0005 0.05 0 0.04)
       , osc WaveSaw (semiPitch 36) 0.18
       , osc WaveSquare (semiPitch 60) 0.10
       ]
       0.20
       (ADSR 0.001 0.13 0 0.16)
       0.64
       (Just filt)
       []

cowbellPatch ∷ Instrument
cowbellPatch =
  let filt = bpFilter 1800 1.3 0.05 0.15 (ADSR 0.001 0.04 0 0.08)
  in drumPatch
       [ osc WaveSquare (semiPitch 19) 0.34
       , osc WaveSquare (semiPitch 31) 0.30
       , osc WaveTriangle (semiPitch 43) 0.16
       ]
        0.12
        (ADSR 0.001 0.11 0 0.13)
        0.72
        (Just filt)
        []

vibraslapPatch ∷ Instrument
vibraslapPatch =
  let buzzEnv = ADSR 0.0005 0.09 0 0.08
      rattleEnv = ADSR 0.001 0.24 0 0.22
      tailEnv = ADSR 0.002 0.34 0 0.30
      filt = bpFilter 2100 1.05 0.10 0.18 (ADSR 0.001 0.08 0 0.12)
  in drumPatch
       [ oscEnv (noiseMix 0.48) rootPitch 0.18 rattleEnv
       , oscEnv WaveSquare (semiPitch 17) 0.18 buzzEnv
       , oscEnv WaveTriangle (semiPitch 31) 0.12 tailEnv
       , oscEnv WaveWhiteNoise rootPitch 0.10 tailEnv
       ]
       0.12
       (ADSR 0.001 0.22 0 0.22)
       0.62
       (Just filt)
       [ envPitch 1 (-10)
       , envPitch 2 (-6)
       ]

bongoPatch ∷ Float → Instrument
bongoPatch variant =
  let filt = bpFilter (900 + 250 * variant) 1.1 0.25 0.28 (ADSR 0.001 0.05 0 0.06)
  in drumPatch
       [ osc WaveTriangle rootPitch 0.52
       , osc WaveSine (semiPitch (7 + 4 * variant)) 0.22
       , osc WaveSquare (semiPitch 19) 0.14
       ]
       0.08
       (ADSR 0.001 0.10 0 0.10)
       0.78
       (Just filt)
       [envPitch 0 (-14)]

congaPatch ∷ Float → Instrument
congaPatch variant =
  let filt = bpFilter (700 + 220 * variant) 1.0 0.30 0.24 (ADSR 0.001 0.06 0 0.08)
  in drumPatch
       [ osc WaveTriangle rootPitch 0.56
       , osc WaveSine (semiPitch (5 + 3 * variant)) 0.20
       , osc WaveSaw (semiPitch 12) 0.14
       ]
       0.08
       (ADSR 0.001 0.14 0 0.12)
       0.78
       (Just filt)
       [envPitch 0 (-12)]
