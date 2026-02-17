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
import Audio.Thread.InstrumentTable (lookupVibrato)

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
            v' <- mixOneVoice srF chunkFrames out rateHz depthCents v
            if eStage (vEnv v') == EnvDone
              then do
                let lastIx = stActiveCount st - 1
                when (i /= lastIx) $
                  MV.read vec lastIx >>= MV.write vec i
                loop i st { stActiveCount = lastIx }
              else MV.write vec i v' >> loop (i+1) st
  loop 0 st0

mixOneVoice ∷ Float → Int → Ptr CFloat → Float → Float → Voice → IO Voice
mixOneVoice srF chunkFrames out vibRateHz vibDepthCents v0 = do
  let framesToDo = chunkFrames
      retuneEvery = 16 ∷ Int
      centsToRatio c = 2 ** (c / 1200)

      go :: Int -> Ptr CFloat -> Voice -> IO Voice
      go i p v
        | i >= framesToDo = pure v
        | otherwise = do
            let hold = vHoldRemain v
                env0 = if hold <= 0 then envRelease (vEnv v) else vEnv v
                (env1, lvl) = envStep srF (vADSR v) env0

            -- vibrato ratio
            let ph0 = vVibPhase v
                vibOn = vibRateHz > 0 && vibDepthCents > 0
                vibCents = if vibOn then vibDepthCents * sin (2*pi*ph0) else 0
                vibRatio = if vibOn then centsToRatio vibCents else 1
                ph1 =
                  if vibOn
                    then let x = ph0 + (vibRateHz / srF)
                         in x - fromIntegral (floor x ∷ Int)
                    else ph0

            -- Step osc layers with hard-sync + spread => stereo
            let nOsc = vOscCount v
            (xL0, xR0) <- stepOscsSync4Stereo srF nOsc vibRatio v
            let xMono = 0.5 * (xL0 + xR0)

            -- filter (mono)
            let (mfilt1, mfenv1, tick1, x1) =
                  case vFilter v of
                    Nothing -> (Nothing, vFiltEnv v, vFiltTick v, xMono)
                    Just fs0 ->
                      let spec = fsSpec fs0
                          tick0 = vFiltTick v
                          tickN = tick0 + 1

                          (mfenvN, envLvl) =
                            if fEnvAmountOct spec == 0 && fQEnvAmount spec == 0
                              then (vFiltEnv v, 0)
                              else case vFiltEnv v of
                                     Nothing -> (Just envInit, 0)
                                     Just fe0 ->
                                       let (fe1, e) = envStep srF (fEnvADSR spec) fe0
                                       in (Just fe1, e)

                          baseCutoff = keyTrackedBaseCutoff (vNoteHz v) spec
                          cutoffHz   = filterEnvCutoff baseCutoff envLvl spec
                          qEff       = if fQEnvAmount spec == 0 then fQ spec else filterEnvQ envLvl spec

                          fsRetuned =
                            if (fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0) && (tickN `mod` retuneEvery == 0)
                              then filterRetune srF cutoffHz qEff fs0
                              else fs0

                          (fs1, y0) = filterStep srF fsRetuned xMono
                      in (Just fs1, mfenvN, tickN, y0)

            let sL = realToFrac (x1 * (vAmpL v * lvl)) :: CFloat
                sR = realToFrac (x1 * (vAmpR v * lvl)) :: CFloat

            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + sL)
            poke (p `advancePtr` 1) (curR + sR)

            let v' = v
                  { vFilter = mfilt1
                  , vFiltEnv = mfenv1
                  , vFiltTick = tick1
                  , vEnv = env1
                  , vHoldRemain = hold - 1
                  , vVibPhase = ph1
                  }

            go (i+1) (p `advancePtr` 2) v'

  go 0 out v0

-- | Step up to 4 oscillator layers, applying hard sync AND per-layer stereo gains.
-- Returns (leftSum, rightSum) before voice amp/env/filter.
stepOscsSync4Stereo ∷ Float → Int → Float → Voice → IO (Float, Float)
stepOscsSync4Stereo srF n vibRatio v = do
  let baseHz = vNoteHz v

  let stepOne li wm0 wm1 wm2 wm3 = do
        osc0 <- MV.read (vOscs v) li
        lvl  <- MV.read (vLevels v) li
        rat  <- MV.read (vPitchRat v) li
        off  <- MV.read (vHzOffset v) li
        m    <- MV.read (vSyncMaster v) li
        gL   <- MV.read (vLayerGainL v) li
        gR   <- MV.read (vLayerGainR v) li

        let masterWrapped =
              case m of
                0 -> wm0
                1 -> wm1
                2 -> wm2
                3 -> wm3
                _ -> False
            oscRst = if m >= 0 && masterWrapped then oscResetPhase0 osc0 else osc0

        let hz = max 0 (baseHz * rat * vibRatio + off)
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
