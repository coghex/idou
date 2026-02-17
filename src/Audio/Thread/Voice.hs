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
import Data.Word (Word64)
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
          osc0 = oscInit WaveSine srF freqHz
          baseInc = hzToPhaseInc srF freqHz

          v = Voice osc0 Nothing Nothing 0 freqHz baseInc holdFrames baseL baseR ampL ampR
                    adsr' env0 Nothing Nothing (stNow st) 0

      MV.write vec n v
      pure st { stActiveCount = n + 1, stNow = stNow st + 1 }

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

applyInstrumentToActiveVoices ∷ Word32 → InstrumentId → Instrument → AudioState → IO AudioState
applyInstrumentToActiveVoices srW iid inst st = do
  let vec = stVoices st
      n   = stActiveCount st
      srF = fromIntegral srW ∷ Float

      needsFiltEnv ∷ FilterSpec → Bool
      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      go i
        | i >= n = pure ()
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v /= Just iid
              then go (i+1)
              else do
                let osc1 = (vOsc v) { oWaveform = iWaveform inst }
                    ampL' = vBaseAmpL v * iGain inst
                    ampR' = vBaseAmpR v * iGain inst
                    adsr' = iAdsrDefault inst

                    (mfilt', mfenv') =
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
                                  Nothing -> filterStateInit srF (vNoteHz v) spec
                                  Just fs0 ->
                                    let baseCutoff = keyTrackedBaseCutoff (vNoteHz v) spec
                                        qEff      = fQ spec
                                        fs1 = fs0 { fsSpec = spec }
                                    in filterRetune srF baseCutoff qEff fs1
                          in (Just fs', env')

                    v' = v { vOsc = osc1, vAmpL = ampL', vAmpR = ampR', vADSR = adsr'
                           , vFilter = mfilt', vFiltEnv = mfenv', vFiltTick = 0
                           }
                MV.write vec i v'
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
          freqHz = noteIdToHz nid
          baseInc = hzToPhaseInc srF freqHz
          holdFrames = maxBound `div` 4
          now' = stNow st + 1

          needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      glideSec <- lookupGlide iid st
      legRetrigFilt <- lookupLegatoFilterRetrig iid st
      legRetrigAmp <- lookupLegatoAmpRetrig iid st
      mLegIx <- findLegatoVoiceIxNewest iid st

      case mLegIx of
        Just ix -> do
          v <- MV.read (stVoices st) ix

          let osc1 = oscSetGlideSec srF glideSec $ oscSetHz srF freqHz (vOsc v)
              ampL' = vBaseAmpL v * iGain inst
              ampR' = vBaseAmpR v * iGain inst
              adsr' = iAdsrDefault inst
              envAmp' = if legRetrigAmp then envInit else vEnv v

              (mfilt', mfenv') =
                case iFilter inst of
                  Nothing -> (Nothing, Nothing)
                  Just spec ->
                    let fs' =
                          case vFilter v of
                            Nothing -> filterStateInit srF freqHz spec
                            Just fs0 ->
                              let baseCutoff = keyTrackedBaseCutoff freqHz spec
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

              v' = v { vOsc = osc1, vNoteHz = freqHz, vBaseInc = baseInc
                     , vNoteId = Just nid, vHoldRemain = holdFrames
                     , vAmpL = ampL', vAmpR = ampR'
                     , vADSR = adsr', vEnv = envAmp'
                     , vFilter = mfilt', vFiltEnv = mfenv', vFiltTick = 0
                     , vStartedAt = now'
                     }

          MV.write (stVoices st) ix v'
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

                  osc0 = oscSetGlideSec srF glideSec $ oscInit (iWaveform inst) srF freqHz

                  filt0 =
                    case iFilter inst of
                      Nothing   -> Nothing
                      Just spec -> Just (filterStateInit srF freqHz spec)

                  filtEnv0 =
                    case iFilter inst of
                      Nothing   -> Nothing
                      Just spec -> if needsFiltEnv spec then Just envInit else Nothing

                  vNew = Voice osc0 filt0 filtEnv0 0 freqHz baseInc holdFrames baseL baseR ampL ampR
                            adsr' env0 (Just nid) (Just iid) now' 0

              MV.write vec n vNew
              pure st { stActiveCount = n + 1, stNow = now' }

panGains ∷ Float → Float → (Float, Float)
panGains amp pan =
  let panClamped = max (-1) (min 1 pan)
      ang = (realToFrac (panClamped + 1) ∷ Double) * (pi/4)
      gL = realToFrac (cos ang) ∷ Float
      gR = realToFrac (sin ang) ∷ Float
  in (amp * gL, amp * gR)
