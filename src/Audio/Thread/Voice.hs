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

noteIdToHz ∷ NoteId → Float
noteIdToHz nid =
  case nid of
    NoteHz hz     -> hz
    NoteMidi midi -> midiToHz midi

midiToHz ∷ Int → Float
midiToHz n = 440 * (2 ** ((fromIntegral n - 69) / 12))

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

mkVoiceLayers
  ∷ Float
  → Float
  → [OscLayer]
  → IO ( MV.IOVector Osc
       , MV.IOVector Float
       , MV.IOVector Float
       , MV.IOVector Float
       , MV.IOVector Int
       , Int
       )
mkVoiceLayers srF baseHz layers = do
  oscs   <- MV.new maxLayers
  levels <- MV.new maxLayers
  rats   <- MV.new maxLayers
  hzOff  <- MV.new maxLayers
  syncM  <- MV.new maxLayers

  let n = length layers

  let go i
        | i >= maxLayers = pure ()
        | i < n =
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
  pure (oscs, levels, rats, hzOff, syncM, max 1 n)

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

      (oscs, levels, rats, hzOff, syncM, oscCount) <-
        mkVoiceLayers srF freqHz [OscLayer WaveSine (PitchSpec 0 0 0 0) 1 NoSync]

      let v = Voice
                { vOscs = oscs
                , vOscCount = oscCount
                , vLevels = levels
                , vPitchRat = rats
                , vHzOffset = hzOff
                , vSyncMaster = syncM
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
                , vNoteId = Nothing
                , vInstrId = Nothing
                , vStartedAt = stNow st
                , vVibPhase = 0
                }

      MV.write vec n v
      pure st { stActiveCount = n + 1, stNow = stNow st + 1 }

applyInstrumentToActiveVoices ∷ Word32 → InstrumentId → Instrument → AudioState → IO AudioState
applyInstrumentToActiveVoices srW iid inst st = do
  let vec = stVoices st
      n   = stActiveCount st
      srF = fromIntegral srW ∷ Float

      layers = normalizeLayers inst
      layerN = length layers

      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      go i
        | i >= n = pure ()
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v /= Just iid
              then go (i+1)
              else do
                let baseHz = vNoteHz v
                let updateLayer li
                      | li >= maxLayers = pure ()
                      | li < layerN =
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
                  { vOscCount = max 1 layerN
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

addInstrumentNote
  ∷ AudioHandle
  → AudioState
  → InstrumentId
  → Float
  → Float
  → NoteId
  → Maybe ADSR
  → IO AudioState
addInstrumentNote h st iid amp pan nid adsrOverride = do
  instM <- lookupInstrument iid st
  case instM of
    Nothing -> pure st
    Just inst -> do
      let srF = fromIntegral (sampleRate h) ∷ Float
          baseHz = noteIdToHz nid
          holdFrames = maxBound `div` 4
          now' = stNow st + 1

          needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      glideSec <- lookupGlide iid st
      legRetrigFilt <- lookupLegatoFilterRetrig iid st
      legRetrigAmp <- lookupLegatoAmpRetrig iid st
      mLegIx <- findLegatoVoiceIxNewest iid st

      let layers = normalizeLayers inst
          layerN = length layers

      case mLegIx of
        Just ix -> do
          v <- MV.read (stVoices st) ix

          let updateLayer li
                | li >= maxLayers = pure ()
                | li < layerN =
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

          let ampL' = vBaseAmpL v * iGain inst
              ampR' = vBaseAmpR v * iGain inst
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
            { vOscCount = max 1 layerN
            , vNoteHz = baseHz
            , vNoteId = Just nid
            , vHoldRemain = holdFrames
            , vAmpL = ampL'
            , vAmpR = ampR'
            , vADSR = adsr'
            , vEnv = envAmp'
            , vFilter = mfilt'
            , vFiltEnv = mfenv'
            , vFiltTick = 0
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
                  ampL = baseL * iGain inst
                  ampR = baseR * iGain inst
                  adsrSpec = maybe (iAdsrDefault inst) id adsrOverride
                  adsr' = adsrSpec { aSustain = clamp01 (aSustain adsrSpec) }
                  env0 = envInit

              (oscs, levels, rats, hzOff, syncM, oscCount) <-
                mkVoiceLayers srF baseHz layers

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
                    , vOscCount = oscCount
                    , vLevels = levels
                    , vPitchRat = rats
                    , vHzOffset = hzOff
                    , vSyncMaster = syncM
                    , vFilter = filt0
                    , vFiltEnv = filtEnv0
                    , vFiltTick = 0
                    , vNoteHz = baseHz
                    , vHoldRemain = holdFrames
                    , vBaseAmpL = baseL
                    , vBaseAmpR = baseR
                    , vAmpL = ampL
                    , vAmpR = ampR
                    , vADSR = adsr'
                    , vEnv = env0
                    , vNoteId = Just nid
                    , vInstrId = Just iid
                    , vStartedAt = now'
                    , vVibPhase = 0
                    }

              MV.write vec n vNew
              pure st { stActiveCount = n + 1, stNow = now' }

releaseInstrumentNote ∷ InstrumentId → NoteId → AudioState → IO AudioState
releaseInstrumentNote iid nid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i
        | i >= n    = pure st
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid && vNoteId v == Just nid
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
