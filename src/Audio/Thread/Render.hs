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
import Audio.Oscillator
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

            let ph0 = vVibPhase v
                vibOn = vibRateHz > 0 && vibDepthCents > 0
                vibCents = if vibOn then vibDepthCents * sin (2*pi*ph0) else 0
                vibRatio = if vibOn then centsToRatio vibCents else 1

                ph1 =
                  if vibOn
                    then let x = ph0 + (vibRateHz / srF)
                         in x - fromIntegral (floor x ∷ Int)
                    else ph0

                o0 = vOsc v
                oVib = o0 { oTargetInc = vBaseInc v * vibRatio }
                (osc1, x0) = oscStep oVib

                (mfilt1, mfenv1, tick1, x1) =
                  case vFilter v of
                    Nothing -> (Nothing, vFiltEnv v, vFiltTick v, x0)
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

                          (fs1, y0) = filterStep srF fsRetuned x0
                      in (Just fs1, mfenvN, tickN, y0)

                sL = realToFrac (x1 * (vAmpL v * lvl)) :: CFloat
                sR = realToFrac (x1 * (vAmpR v * lvl)) :: CFloat

            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + sL)
            poke (p `advancePtr` 1) (curR + sR)

            let v' = v
                  { vOsc = osc1
                  , vFilter = mfilt1
                  , vFiltEnv = mfenv1
                  , vFiltTick = tick1
                  , vEnv = env1
                  , vHoldRemain = hold - 1
                  , vVibPhase = ph1
                  }

            go (i+1) (p `advancePtr` 2) v'
  go 0 out v0
