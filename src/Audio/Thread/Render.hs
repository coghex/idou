{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread.Render
  ( renderIfNeeded
  , renderChunkFrames
  ) where

import Control.Monad (when)
import Data.Word (Word32)
import Foreign
import Foreign.C
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)

import qualified Data.Vector.Mutable as MV

import Audio.Envelope
import Audio.Filter
import Audio.Oscillator (oscSetHz, oscStepWrap, oscResetPhase0)
import Sound.Miniaudio.RingBuffer

import Audio.Thread.Types
import Audio.Thread.InstrumentTable (lookupVibrato, lookupInstrument)
import Audio.Types (Instrument(..), ModRoute(..), ModSrc(..), ModDst(..))

renderIfNeeded ∷ Ptr MaRB → Int → AudioState → IO AudioState
renderIfNeeded rb chunkFrames st0 = do
  let targetFrames = fromIntegral (chunkFrames * 4) ∷ Word32
      loop st = do
        availRead  <- rbAvailableRead rb
        availWrite <- rbAvailableWrite rb
        if availRead >= targetFrames || availWrite == 0
          then pure st
          else do
            let framesNow = min chunkFrames (fromIntegral availWrite)
            st' <- renderChunkFrames rb framesNow st
            loop st'
  loop st0

renderChunkFrames ∷ Ptr MaRB → Int → AudioState → IO AudioState
renderChunkFrames rb framesNow st = do
  st' <- withForeignPtr (stMixBuf st) $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
        samples = framesNow * 2
        bytes = samples * sizeOf (undefined ∷ CFloat)
    fillBytes out 0 bytes
    mixVoices (fromIntegral sr) framesNow out st

  withForeignPtr (stMixBuf st') $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
    _ <- rbWriteF32 rb out (fromIntegral framesNow)
    pure ()

  pure st'

mixVoices ∷ Float → Int → Ptr CFloat → AudioState → IO AudioState
mixVoices srF chunkFrames out st0 = do
  let vec = stVoices st0
      loop i st
        | i >= stActiveCount st = pure st
        | otherwise = do
            v <- MV.read vec i
            (rateHz, depthCents) <-
              case vInstrId v of
                Nothing  -> pure (0,0)
                Just iid -> lookupVibrato iid st
            v' <- mixOneVoice srF chunkFrames out rateHz depthCents st v
            if eStage (vEnv v') == EnvDone
              then do
                let lastIx = stActiveCount st - 1
                when (i /= lastIx) $
                  MV.read vec lastIx >>= MV.write vec i
                loop i st { stActiveCount = lastIx }
              else MV.write vec i v' >> loop (i+1) st
  loop 0 st0

mixOneVoice ∷ Float → Int → Ptr CFloat → Float → Float → AudioState → Voice → IO Voice
mixOneVoice srF chunkFrames out vibRateHz vibDepthCents st v0 = do
  let framesToDo = chunkFrames
      retuneEvery = 16 ∷ Int
      modEvery = 16 ∷ Int
      centsToRatio c = 2 ** (c / 1200)
      maxRoutes = 8 ∷ Int

  pitchModRat0 <- MV.replicate maxLayers 1

  let go :: Int -> Ptr CFloat -> Int -> Float -> Voice -> IO Voice
      go i p tick filtModOct v
        | i >= framesToDo = pure v
        | otherwise = do
            let hold = vHoldRemain v
                env0 = if hold <= 0 then envRelease (vEnv v) else vEnv v
                (env1, envAmp) = envStep srF (vADSR v) env0

            -- vibrato ratio (pitch vibrato)
            let phV0 = vVibPhase v
                vibOn = vibRateHz > 0 && vibDepthCents > 0
                vibCents = if vibOn then vibDepthCents * sin (2*pi*phV0) else 0
                vibRatio = if vibOn then centsToRatio vibCents else 1
                phV1 =
                  if vibOn
                    then let x = phV0 + (vibRateHz / srF)
                         in x - fromIntegral (floor x ∷ Int)
                    else phV0

            -- LFO1 phase (uses same rate as AudioSetVibrato)
            let phL0 = vLfo1Phase v
                phL1 =
                  if vibRateHz > 0
                    then let x = phL0 + (vibRateHz / srF)
                         in x - fromIntegral (floor x ∷ Int)
                    else phL0

            -- control-rate: update pitchModRat0 and filtModOct, plus amp mod
            (ampModMul, filtModOct', v') <-
              if tick `mod` modEvery == 0
                then do
                  (ampMul, fOct) <- computeMods maxRoutes st vibRateHz phL1 envAmp v pitchModRat0
                  pure (ampMul, fOct, v)
                else pure (1, filtModOct, v)

            let nOsc = vOscCount v'
            (xL0, xR0) <- stepOscsSync4StereoMod srF nOsc vibRatio pitchModRat0 v'
            let xMono = 0.5 * (xL0 + xR0)

            let (mfilt1, mfenv1, tick1, x1) =
                  case vFilter v' of
                    Nothing -> (Nothing, vFiltEnv v', vFiltTick v', xMono)
                    Just fs0 ->
                      let spec = fsSpec fs0
                          tick0 = vFiltTick v'
                          tickN = tick0 + 1

                          (mfenvN, envLvl) =
                            if fEnvAmountOct spec == 0 && fQEnvAmount spec == 0
                              then (vFiltEnv v', 0)
                              else case vFiltEnv v' of
                                     Nothing -> (Just envInit, 0)
                                     Just fe0 ->
                                       let (fe1, e) = envStep srF (fEnvADSR spec) fe0
                                       in (Just fe1, e)

                          baseCutoff = keyTrackedBaseCutoff (vNoteHz v') spec
                          cutoffHz0  = filterEnvCutoff baseCutoff envLvl spec
                          cutoffHz   = cutoffHz0 * (2 ** filtModOct')
                          qEff       = if fQEnvAmount spec == 0 then fQ spec else filterEnvQ envLvl spec

                          fsRetuned =
                            if (fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0 || filtModOct' /= 0) && (tickN `mod` retuneEvery == 0)
                              then filterRetune srF cutoffHz qEff fs0
                              else fs0

                          (fs1, y0) = filterStep srF fsRetuned xMono
                      in (Just fs1, mfenvN, tickN, y0)

            let ampL = vAmpL v' * ampModMul
                ampR = vAmpR v' * ampModMul
                sL = realToFrac (x1 * (ampL * envAmp)) :: CFloat
                sR = realToFrac (x1 * (ampR * envAmp)) :: CFloat

            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + sL)
            poke (p `advancePtr` 1) (curR + sR)

            let vNext = v'
                  { vFilter = mfilt1
                  , vFiltEnv = mfenv1
                  , vFiltTick = tick1
                  , vEnv = env1
                  , vHoldRemain = hold - 1
                  , vVibPhase = phV1
                  , vLfo1Phase = phL1
                  }

            go (i+1) (p `advancePtr` 2) (tick+1) filtModOct' vNext

  go 0 out 0 0 v0

-- Compute mod routes into:
--  * pitchModRatOut: per-layer multiplicative ratio (written into provided vector)
--  * filtModOct: additive octaves
--  * ampMul: linear amplitude multiplier (>=0)
computeMods
  :: Int
  -> AudioState
  -> Float          -- lfo rate hz (vibRateHz)
  -> Float          -- lfo phase [0..1]
  -> Float          -- envAmp [0..1]
  -> Voice
  -> MV.IOVector Float  -- pitchModRatOut (len maxLayers), written
  -> IO (Float, Float)
computeMods maxRoutes st _lfoRateHz lfoPhase envAmp v pitchModRatOut = do
  -- sources
  let lfo1 = sin (2*pi*lfoPhase)          -- [-1..1]
      keyTrack = keyTrack01 (vNoteHz v)   -- [0..1]
      envFilt = case vFiltEnv v of
                  Nothing -> 0
                  Just fe -> eLevel fe

  routes <-
    case vInstrId v of
      Nothing -> pure []
      Just iid -> do
        mi <- lookupInstrument iid st
        pure $ case mi of
          Nothing -> []
          Just inst -> take maxRoutes (iModRoutes inst)

  pitchCents <- MV.replicate maxLayers 0

  let addPitchCents ix c =
        when (ix >= 0 && ix < maxLayers) $ do
          cur <- MV.read pitchCents ix
          MV.write pitchCents ix (cur + c)

  let loop rs ampAcc filtAcc =
        case rs of
          [] -> pure (ampAcc, filtAcc)
          (ModRoute src dst amt : xs) -> do
            let s =
                  case src of
                    ModSrcLfo1      -> lfo1
                    ModSrcEnvAmp    -> envAmp
                    ModSrcEnvFilter -> envFilt
                    ModSrcKeyTrack  -> keyTrack
            (ampAcc', filtAcc') <-
              case dst of
                ModDstLayerPitchCents ix -> addPitchCents ix (amt * s) >> pure (ampAcc, filtAcc)
                ModDstFilterCutoffOct    -> pure (ampAcc, filtAcc + (amt * s))
                ModDstAmpGain            -> pure (ampAcc + (amt * s), filtAcc)
            loop xs ampAcc' filtAcc'

  (ampDelta, filtOct) <- loop routes 0 0

  let centsToRatio c = 2 ** (c / 1200)
  let goIx i
        | i >= maxLayers = pure ()
        | otherwise = do
            c <- MV.read pitchCents i
            MV.write pitchModRatOut i (centsToRatio c)
            goIx (i+1)
  goIx 0

  let ampMul = max 0 (1 + ampDelta)
  pure (ampMul, filtOct)

keyTrack01 :: Float -> Float
keyTrack01 hz =
  let h = max 1 hz
      x = (logBase 2 h - logBase 2 55) / (logBase 2 1760 - logBase 2 55)
  in max 0 (min 1 x)

stepOscsSync4StereoMod
  :: Float -> Int -> Float -> MV.IOVector Float -> Voice -> IO (Float, Float)
stepOscsSync4StereoMod srF n vibRatio pitchModRat v = do
  let baseHz = vNoteHz v

  let stepOne li wm0 wm1 wm2 wm3 = do
        osc0 <- MV.read (vOscs v) li
        lvl  <- MV.read (vLevels v) li
        rat  <- MV.read (vPitchRat v) li
        off  <- MV.read (vHzOffset v) li
        m    <- MV.read (vSyncMaster v) li
        gL   <- MV.read (vLayerGainL v) li
        gR   <- MV.read (vLayerGainR v) li
        mr   <- MV.read pitchModRat li

        let masterWrapped =
              case m of
                0 -> wm0
                1 -> wm1
                2 -> wm2
                3 -> wm3
                _ -> False
            oscRst = if m >= 0 && masterWrapped then oscResetPhase0 osc0 else osc0

        let hz = max 0 (baseHz * rat * vibRatio * mr + off)
            oscT = oscSetHz srF hz oscRst
            (osc1, x, wrapped) = oscStepWrap oscT

        MV.write (vOscs v) li osc1
        let xScaled = lvl * x
        pure ((xScaled * gL, xScaled * gR), wrapped)

  ((x0L, x0R), w0) <-
    if n > 0 then stepOne 0 False False False False else pure ((0,0), False)
  ((x1L, x1R), w1) <-
    if n > 1 then stepOne 1 w0 False False False else pure ((0,0), False)
  ((x2L, x2R), w2) <-
    if n > 2 then stepOne 2 w0 w1 False False else pure ((0,0), False)
  ((x3L, x3R), _w3) <-
    if n > 3 then stepOne 3 w0 w1 w2 False else pure ((0,0), False)

  pure (x0L + x1L + x2L + x3L, x0R + x1R + x2R + x3R)
