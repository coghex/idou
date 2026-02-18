{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread.Voice
  ( noteIdToHz
  , addBeepVoice
  , addInstrumentNote
  , releaseInstrumentNote
  , releaseInstrumentAllVoices
  , applyInstrumentToActiveVoices
  , findLegatoVoiceIxNewest
  , panGains
  ) where

import Control.Monad (when)
import Data.Word (Word32)
import qualified Data.Vector.Mutable as MV

import Audio.Types
import Audio.Envelope
import Audio.Oscillator
import Audio.Filter
  ( FilterState(..)
  , filterStateInit
  , filterRetune
  , keyTrackedBaseCutoff
  )
import Audio.Filter.Types (FilterSpec(..))

import Audio.Thread.Types
import Audio.Thread.InstrumentTable
  ( lookupInstrument
  , lookupGlide
  , lookupLegatoFilterRetrig
  , lookupLegatoAmpRetrig
  )

-- Keep for beeps only
noteIdToHz ∷ NoteId → Float
noteIdToHz nid =
  case nid of
    NoteHz hz     -> hz
    NoteMidi midi -> midiToHz midi

midiToHz ∷ Int → Float
midiToHz n = 440 * (2 ** ((fromIntegral n - 69) / 12))

noteKeyToHz ∷ NoteKey → Float
noteKeyToHz (NoteKey k) = midiToHz k

pitchSpecRatio ∷ PitchSpec → Float
pitchSpecRatio ps =
  let oct = fromIntegral (psOctaves ps) ∷ Float
      semi = psSemitones ps
      cents = psCents ps
  in (2 ** oct) * (2 ** (semi / 12)) * (2 ** (cents / 1200))

normalizeLayers ∷ Instrument → [OscLayer]
normalizeLayers inst =
  case iOscs inst of
    [] ->
      [ OscLayer WaveSine (PitchSpec 0 0 0 0) 1 NoSync ]
    xs -> take maxLayers xs

layerMasterIx ∷ Int → SyncSpec → Int
layerMasterIx layerIx s =
  case s of
    NoSync -> -1
    HardSyncTo m ->
      if m >= 0 && m < layerIx then m else -1

clamp01' :: Float -> Float
clamp01' x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

layerBasePan :: Int -> Int -> Float
layerBasePan n i =
  case n of
    1 -> 0
    2 -> if i == 0 then (-1) else 1
    3 -> case i of
           0 -> (-1)
           1 -> 0
           _ -> 1
    _ -> case i of
           0 -> (-1)
           1 -> (-0.33)
           2 -> 0.33
           _ -> 1

panGainsCP :: Float -> (Float, Float)
panGainsCP pan =
  let p = max (-1) (min 1 pan)
      ang = (realToFrac (p + 1) ∷ Double) * (pi/4)
      gL = realToFrac (cos ang) ∷ Float
      gR = realToFrac (sin ang) ∷ Float
  in (gL, gR)

writeLayerGains :: Float -> Int -> MV.IOVector Float -> MV.IOVector Float -> IO ()
writeLayerGains spread n gL gR = do
  let s = clamp01' spread
      go i
        | i >= maxLayers = pure ()
        | i < n = do
            let base = layerBasePan n i
                pan  = base * s
                (l,r) = panGainsCP pan
            MV.write gL i l
            MV.write gR i r
            go (i+1)
        | otherwise = do
            MV.write gL i 0
            MV.write gR i 0
            go (i+1)
  go 0

mkVoiceLayers
  ∷ Float
  → Float
  → Float
  → [OscLayer]
  → IO ( MV.IOVector Osc
       , MV.IOVector Float
       , MV.IOVector Float
       , MV.IOVector Float
       , MV.IOVector Int
       , MV.IOVector Float
       , MV.IOVector Float
       , Int
       )
mkVoiceLayers srF baseHz spread layers = do
  oscs   <- MV.new maxLayers
  levels <- MV.new maxLayers
  rats   <- MV.new maxLayers
  hzOff  <- MV.new maxLayers
  syncM  <- MV.new maxLayers
  gL     <- MV.new maxLayers
  gR     <- MV.new maxLayers

  let n0 = length layers
      n  = max 1 n0

  writeLayerGains spread n gL gR

  let go i
        | i >= maxLayers = pure ()
        | i < n0 =
            case layers !! i of
              OscLayer wf ps lvl syncSpec -> do
                let ratio = pitchSpecRatio ps
                    hz    = baseHz * ratio + psHzOffset ps
                    master = layerMasterIx i syncSpec
                MV.write oscs i (oscInit wf srF (max 0 hz))
                MV.write levels i lvl
                MV.write rats i ratio
                MV.write hzOff i (psHzOffset ps)
                MV.write syncM i master
                go (i+1)
        | otherwise = do
            MV.write oscs i (oscInit WaveSine srF 0)
            MV.write levels i 0
            MV.write rats i 1
            MV.write hzOff i 0
            MV.write syncM i (-1)
            go (i+1)

  go 0
  pure (oscs, levels, rats, hzOff, syncM, gL, gR, n)

findLegatoVoiceIxNewest ∷ InstrumentId → AudioState → IO (Maybe Int)
findLegatoVoiceIxNewest iid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i bestIx bestStamp
        | i >= n = pure bestIx
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid && eStage (vEnv v) /= EnvRelease && eStage (vEnv v) /= EnvDone
              then if vStartedAt v >= bestStamp
                     then go (i+1) (Just i) (vStartedAt v)
                     else go (i+1) bestIx bestStamp
              else go (i+1) bestIx bestStamp
  go 0 Nothing 0

-- Poly helpers ---------------------------------------------------------------

countInstrumentVoices ∷ InstrumentId → AudioState → IO Int
countInstrumentVoices iid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i acc
        | i >= n = pure acc
        | otherwise = do
            v <- MV.read vec i
            let ok = vInstrId v == Just iid && eStage (vEnv v) /= EnvDone
            go (i+1) (if ok then acc+1 else acc)
  go 0 0

findStealVoiceIxQuietest ∷ InstrumentId → AudioState → IO (Maybe Int)
findStealVoiceIxQuietest iid st = do
  let vec = stVoices st
      n   = stActiveCount st

      loudness v =
        let envLvl = max 0 (eLevel (vEnv v))
            amp    = 0.5 * (abs (vAmpL v) + abs (vAmpR v))
        in envLvl * amp

      go i bestIx bestVal onlyRelease
        | i >= n = pure bestIx
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v /= Just iid || eStage (vEnv v) == EnvDone
              then go (i+1) bestIx bestVal onlyRelease
              else do
                let isRel = eStage (vEnv v) == EnvRelease
                    ok = (not onlyRelease) || isRel
                if not ok
                  then go (i+1) bestIx bestVal onlyRelease
                  else do
                    let l = loudness v
                    if bestIx == Nothing || l < bestVal
                      then go (i+1) (Just i) l onlyRelease
                      else go (i+1) bestIx bestVal onlyRelease

  r1 <- go 0 Nothing 1e30 True
  case r1 of
    Just _ -> pure r1
    Nothing -> go 0 Nothing 1e30 False

-- Voice creation -------------------------------------------------------------

addBeepVoice ∷ AudioHandle → AudioState → Float → Float → Float → Float → ADSR → IO AudioState
addBeepVoice h st amp pan freqHz durSec adsrSpec = do
  let n = stActiveCount st
      vec = stVoices st
      cap = MV.length vec
  if n >= cap
    then pure st
    else do
      let srF = fromIntegral (sampleRate h) ∷ Float
          holdFrames = max 0 (floor (realToFrac durSec * srF) ∷ Int)
          (baseL, baseR) = panGains amp pan
          ampL = baseL
          ampR = baseR
          adsr' = adsrSpec { aSustain = clamp01 (aSustain adsrSpec) }
          env0 = envInit

      (oscs, levels, rats, hzOff, syncM, gL, gR, oscCount) <-
        mkVoiceLayers srF freqHz 0 [OscLayer WaveSine (PitchSpec 0 0 0 0) 1 NoSync]

      let v = Voice
                { vOscs = oscs
                , vOscCount = oscCount
                , vLevels = levels
                , vPitchRat = rats
                , vHzOffset = hzOff
                , vSyncMaster = syncM
                , vLayerGainL = gL
                , vLayerGainR = gR
                , vFilter = Nothing
                , vFiltEnv = Nothing
                , vFiltTick = 0
                , vNoteHz = freqHz
                , vHoldRemain = holdFrames
                , vBaseAmpL = baseL
                , vBaseAmpR = baseR
                , vAmpL = ampL
                , vAmpR = ampR
                , vADSR = adsr'
                , vEnv = env0
                , vNoteKey = Nothing
                , vNoteInstanceId = Nothing
                , vVelocity = 1
                , vInstrId = Nothing
                , vStartedAt = stNow st
                , vVibPhase = 0
                , vLfo1Phase = 0
                }

      MV.write vec n v
      pure st { stActiveCount = n + 1, stNow = stNow st + 1 }

applyInstrumentToActiveVoices ∷ Word32 → InstrumentId → Instrument → AudioState → IO AudioState
applyInstrumentToActiveVoices srW iid inst st = do
  let vec = stVoices st
      n   = stActiveCount st
      srF = fromIntegral srW ∷ Float

      layers = normalizeLayers inst
      layerN0 = length layers
      layerN  = max 1 layerN0
      spread  = clamp01' (iLayerSpread inst)

      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      go i
        | i >= n = pure ()
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v /= Just iid
              then go (i+1)
              else do
                let baseHz = vNoteHz v

                writeLayerGains spread layerN (vLayerGainL v) (vLayerGainR v)

                let updateLayer li
                      | li >= maxLayers = pure ()
                      | li < layerN0 =
                          case layers !! li of
                            OscLayer wf ps lvl syncSpec -> do
                              osc0 <- MV.read (vOscs v) li
                              MV.write (vOscs v) li (osc0 { oWaveform = wf })
                              MV.write (vLevels v) li lvl
                              MV.write (vPitchRat v) li (pitchSpecRatio ps)
                              MV.write (vHzOffset v) li (psHzOffset ps)
                              MV.write (vSyncMaster v) li (layerMasterIx li syncSpec)
                              updateLayer (li+1)
                      | otherwise = do
                          MV.write (vLevels v) li 0
                          MV.write (vPitchRat v) li 1
                          MV.write (vHzOffset v) li 0
                          MV.write (vSyncMaster v) li (-1)
                          updateLayer (li+1)

                updateLayer 0

                -- apply iGain live; keep velocity baked into vBaseAmp* already
                let ampL' = vBaseAmpL v * iGain inst
                    ampR' = vBaseAmpR v * iGain inst
                    adsr' = iAdsrDefault inst

                let (mfilt', mfenv') =
                      case iFilter inst of
                        Nothing -> (Nothing, Nothing)
                        Just spec ->
                          let env' =
                                if needsFiltEnv spec
                                  then case vFiltEnv v of
                                         Nothing -> Just envInit
                                         Just fe -> Just fe
                                  else Nothing
                              fs' =
                                case vFilter v of
                                  Nothing -> filterStateInit srF baseHz spec
                                  Just fs0 ->
                                    let baseCutoff = keyTrackedBaseCutoff baseHz spec
                                        qEff      = fQ spec
                                        fs1 = fs0 { fsSpec = spec }
                                    in filterRetune srF baseCutoff qEff fs1
                          in (Just fs', env')

                MV.write vec i v
                  { vOscCount = layerN
                  , vAmpL = ampL'
                  , vAmpR = ampR'
                  , vADSR = adsr'
                  , vFilter = mfilt'
                  , vFiltEnv = mfenv'
                  , vFiltTick = 0
                  }

                go (i+1)

  go 0
  pure st

-- Main NoteOn entrypoint ------------------------------------------------------

addInstrumentNote
  ∷ AudioHandle
  → AudioState
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO AudioState
addInstrumentNote h st iid amp pan key instId vel adsrOverride = do
  instM <- lookupInstrument iid st
  case instM of
    Nothing -> pure st
    Just inst ->
      case iPlayMode inst of
        MonoLegato -> addInstrumentNoteMonoLegato h st iid inst amp pan key instId vel adsrOverride
        Poly       -> addInstrumentNotePoly       h st iid inst amp pan key instId vel adsrOverride

-- Mono-legato path ------------------------------------------------------------

addInstrumentNoteMonoLegato
  ∷ AudioHandle → AudioState → InstrumentId → Instrument
  → Float → Float → NoteKey → NoteInstanceId → Float → Maybe ADSR
  → IO AudioState
addInstrumentNoteMonoLegato h st iid inst amp pan key instId vel _adsrOverride = do
  let srF = fromIntegral (sampleRate h) ∷ Float
      baseHz = noteKeyToHz key
      holdFrames = maxBound `div` 4
      now' = stNow st + 1
      vel' = clamp01' vel

      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

  glideSec <- lookupGlide iid st
  legRetrigFilt <- lookupLegatoFilterRetrig iid st
  legRetrigAmp <- lookupLegatoAmpRetrig iid st
  mLegIx <- findLegatoVoiceIxNewest iid st

  let layers = normalizeLayers inst
      layerN0 = length layers
      layerN  = max 1 layerN0
      spread  = clamp01' (iLayerSpread inst)

  case mLegIx of
    Just ix -> do
      v <- MV.read (stVoices st) ix
      writeLayerGains spread layerN (vLayerGainL v) (vLayerGainR v)

      let updateLayer li
            | li >= maxLayers = pure ()
            | li < layerN0 =
                case layers !! li of
                  OscLayer wf ps lvl syncSpec -> do
                    let ratio = pitchSpecRatio ps
                        hz    = baseHz * ratio + psHzOffset ps
                    osc0 <- MV.read (vOscs v) li
                    let osc1 = oscSetGlideSec srF glideSec
                             $ oscSetHz srF (max 0 hz)
                             $ (osc0 { oWaveform = wf })
                    MV.write (vOscs v) li osc1
                    MV.write (vLevels v) li lvl
                    MV.write (vPitchRat v) li ratio
                    MV.write (vHzOffset v) li (psHzOffset ps)
                    MV.write (vSyncMaster v) li (layerMasterIx li syncSpec)
                    updateLayer (li+1)
            | otherwise = do
                MV.write (vLevels v) li 0
                MV.write (vPitchRat v) li 1
                MV.write (vHzOffset v) li 0
                MV.write (vSyncMaster v) li (-1)
                updateLayer (li+1)

      updateLayer 0

      let (baseL, baseR) = panGains amp pan
          baseL' = baseL * vel'
          baseR' = baseR * vel'
          ampL' = baseL' * iGain inst
          ampR' = baseR' * iGain inst
          adsr' = iAdsrDefault inst
          envAmp' = if legRetrigAmp then envInit else vEnv v

          (mfilt', mfenv') =
            case iFilter inst of
              Nothing -> (Nothing, Nothing)
              Just spec ->
                let fs' =
                      case vFilter v of
                        Nothing -> filterStateInit srF baseHz spec
                        Just fs0 ->
                          let baseCutoff = keyTrackedBaseCutoff baseHz spec
                              qEff = fQ spec
                              fs1 = fs0 { fsSpec = spec }
                          in filterRetune srF baseCutoff qEff fs1

                    env' =
                      if needsFiltEnv spec
                        then if legRetrigFilt
                               then Just envInit
                               else case vFiltEnv v of
                                      Nothing -> Just envInit
                                      Just fe -> Just fe
                        else Nothing
                in (Just fs', env')

      MV.write (stVoices st) ix v
        { vOscCount = layerN
        , vNoteHz = baseHz
        , vHoldRemain = holdFrames
        , vBaseAmpL = baseL'
        , vBaseAmpR = baseR'
        , vAmpL = ampL'
        , vAmpR = ampR'
        , vADSR = adsr'
        , vEnv = envAmp'
        , vFilter = mfilt'
        , vFiltEnv = mfenv'
        , vFiltTick = 0
        , vNoteKey = Just key
        , vNoteInstanceId = Just instId
        , vVelocity = vel'
        , vStartedAt = now'
        }

      pure st { stNow = now' }

    Nothing -> do
      let n = stActiveCount st
          vec = stVoices st
          cap = MV.length vec
      if n >= cap
        then pure st { stNow = now' }
        else do
          let (baseL, baseR) = panGains amp pan
              baseL' = baseL * vel'
              baseR' = baseR * vel'
              ampL = baseL' * iGain inst
              ampR = baseR' * iGain inst
              adsr' = (iAdsrDefault inst) { aSustain = clamp01 (aSustain (iAdsrDefault inst)) }
              env0 = envInit

          (oscs, levels, rats, hzOff, syncM, gL, gR, oscCount) <-
            mkVoiceLayers srF baseHz spread layers

          let initLayer li
                | li >= oscCount = pure ()
                | otherwise = do
                    osc0 <- MV.read oscs li
                    MV.write oscs li (oscSetGlideSec srF glideSec osc0)
                    initLayer (li+1)
          initLayer 0

          let filt0 =
                case iFilter inst of
                  Nothing   -> Nothing
                  Just spec -> Just (filterStateInit srF baseHz spec)

              filtEnv0 =
                case iFilter inst of
                  Nothing   -> Nothing
                  Just spec -> if needsFiltEnv spec then Just envInit else Nothing

          let vNew = Voice
                { vOscs = oscs
                , vOscCount = layerN
                , vLevels = levels
                , vPitchRat = rats
                , vHzOffset = hzOff
                , vSyncMaster = syncM
                , vLayerGainL = gL
                , vLayerGainR = gR
                , vFilter = filt0
                , vFiltEnv = filtEnv0
                , vFiltTick = 0
                , vNoteHz = baseHz
                , vHoldRemain = holdFrames
                , vBaseAmpL = baseL'
                , vBaseAmpR = baseR'
                , vAmpL = ampL
                , vAmpR = ampR
                , vADSR = adsr'
                , vEnv = env0
                , vNoteKey = Just key
                , vNoteInstanceId = Just instId
                , vVelocity = vel'
                , vInstrId = Just iid
                , vStartedAt = now'
                , vVibPhase = 0
                , vLfo1Phase = 0
                }

          MV.write vec n vNew
          pure st { stActiveCount = n + 1, stNow = now' }

-- Poly path -------------------------------------------------------------------

addInstrumentNotePoly
  ∷ AudioHandle → AudioState → InstrumentId → Instrument
  → Float → Float → NoteKey → NoteInstanceId → Float → Maybe ADSR
  → IO AudioState
addInstrumentNotePoly h st iid inst amp pan key instId vel adsrOverride = do
  let srF = fromIntegral (sampleRate h) ∷ Float
      baseHz = noteKeyToHz key
      holdFrames = maxBound `div` 4
      now' = stNow st + 1
      vel' = clamp01' vel
      layers = normalizeLayers inst
      layerN0 = length layers
      layerN  = max 1 layerN0
      spread  = clamp01' (iLayerSpread inst)

      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

  glideSec <- lookupGlide iid st
  let polyMax = max 1 (iPolyMax inst)

  activeForInst <- countInstrumentVoices iid st

  let vec = stVoices st
      cap = MV.length vec
      nActive = stActiveCount st

  let mkNewVoice = do
        let (baseL, baseR) = panGains amp pan
            baseL' = baseL * vel'
            baseR' = baseR * vel'
            ampL = baseL' * iGain inst
            ampR = baseR' * iGain inst
            adsrSpec = maybe (iAdsrDefault inst) id adsrOverride
            adsr' = adsrSpec { aSustain = clamp01 (aSustain adsrSpec) }
            env0 = envInit

        (oscs, levels, rats, hzOff, syncM, gL, gR, oscCount) <-
          mkVoiceLayers srF baseHz spread layers

        let initLayer li
              | li >= oscCount = pure ()
              | otherwise = do
                  osc0 <- MV.read oscs li
                  MV.write oscs li (oscSetGlideSec srF glideSec osc0)
                  initLayer (li+1)
        initLayer 0

        let filt0 =
              case iFilter inst of
                Nothing   -> Nothing
                Just spec -> Just (filterStateInit srF baseHz spec)

            filtEnv0 =
              case iFilter inst of
                Nothing   -> Nothing
                Just spec -> if needsFiltEnv spec then Just envInit else Nothing

        pure Voice
          { vOscs = oscs
          , vOscCount = layerN
          , vLevels = levels
          , vPitchRat = rats
          , vHzOffset = hzOff
          , vSyncMaster = syncM
          , vLayerGainL = gL
          , vLayerGainR = gR
          , vFilter = filt0
          , vFiltEnv = filtEnv0
          , vFiltTick = 0
          , vNoteHz = baseHz
          , vHoldRemain = holdFrames
          , vBaseAmpL = baseL'
          , vBaseAmpR = baseR'
          , vAmpL = ampL
          , vAmpR = ampR
          , vADSR = adsr'
          , vEnv = env0
          , vNoteKey = Just key
          , vNoteInstanceId = Just instId
          , vVelocity = vel'
          , vInstrId = Just iid
          , vStartedAt = now'
          , vVibPhase = 0
          , vLfo1Phase = 0
          }

  if activeForInst < polyMax && nActive < cap
    then do
      vNew <- mkNewVoice
      MV.write vec nActive vNew
      pure st { stActiveCount = nActive + 1, stNow = now' }
    else do
      mIx <- findStealVoiceIxQuietest iid st
      case mIx of
        Nothing ->
          pure st { stNow = now' }
        Just ix -> do
          vNew <- mkNewVoice
          MV.write vec ix vNew
          pure st { stNow = now' }

-- Releases --------------------------------------------------------------------

releaseInstrumentNote ∷ InstrumentId → NoteInstanceId → AudioState → IO AudioState
releaseInstrumentNote iid instId st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i
        | i >= n    = pure st
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid && vNoteInstanceId v == Just instId
              then do
                let fe' = fmap envRelease (vFiltEnv v)
                MV.write vec i v { vEnv = envRelease (vEnv v), vFiltEnv = fe' }
              else pure ()
            go (i+1)
  go 0

releaseInstrumentAllVoices ∷ InstrumentId → AudioState → IO AudioState
releaseInstrumentAllVoices iid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i
        | i >= n = pure st
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid
              then do
                let fe' = fmap envRelease (vFiltEnv v)
                MV.write vec i v { vEnv = envRelease (vEnv v), vFiltEnv = fe', vHoldRemain = 0 }
              else pure ()
            go (i+1)
  go 0

panGains ∷ Float → Float → (Float, Float)
panGains amp pan =
  let panClamped = max (-1) (min 1 pan)
      ang = (realToFrac (panClamped + 1) ∷ Double) * (pi/4)
      gL = realToFrac (cos ang) ∷ Float
      gR = realToFrac (sin ang) ∷ Float
  in (amp * gL, amp * gR)
