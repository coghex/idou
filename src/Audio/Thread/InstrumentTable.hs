{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Thread.InstrumentTable
  ( setInstrument
  , lookupInstrument
  , setGlide
  , lookupGlide
  , setLegatoFilterRetrig
  , lookupLegatoFilterRetrig
  , setLegatoAmpRetrig
  , lookupLegatoAmpRetrig
  , setVibrato
  , lookupVibrato
  ) where

import qualified Data.Vector.Mutable as MV

import Audio.Types (InstrumentId(..), Instrument(..))
import Audio.Thread.Types (AudioState(..))

setInstrument ∷ InstrumentId → Instrument → AudioState → IO AudioState
setInstrument (InstrumentId i) inst st = do
  let vec = stInstruments st
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i (Just inst) >> pure st

lookupInstrument ∷ InstrumentId → AudioState → IO (Maybe Instrument)
lookupInstrument (InstrumentId i) st = do
  let vec = stInstruments st
  if i < 0 || i >= MV.length vec then pure Nothing else MV.read vec i

setGlide ∷ InstrumentId → Float → AudioState → IO AudioState
setGlide (InstrumentId i) gs st = do
  let vec = stGlideSec st
      gs' = max 0 gs
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i gs' >> pure st

lookupGlide ∷ InstrumentId → AudioState → IO Float
lookupGlide (InstrumentId i) st = do
  let vec = stGlideSec st
  if i < 0 || i >= MV.length vec then pure 0 else MV.read vec i

setLegatoFilterRetrig ∷ InstrumentId → Bool → AudioState → IO AudioState
setLegatoFilterRetrig (InstrumentId i) rf st = do
  let vec = stLegFiltRetrig st
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i rf >> pure st

lookupLegatoFilterRetrig ∷ InstrumentId → AudioState → IO Bool
lookupLegatoFilterRetrig (InstrumentId i) st = do
  let vec = stLegFiltRetrig st
  if i < 0 || i >= MV.length vec then pure False else MV.read vec i

setLegatoAmpRetrig ∷ InstrumentId → Bool → AudioState → IO AudioState
setLegatoAmpRetrig (InstrumentId i) ra st = do
  let vec = stLegAmpRetrig st
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i ra >> pure st

lookupLegatoAmpRetrig ∷ InstrumentId → AudioState → IO Bool
lookupLegatoAmpRetrig (InstrumentId i) st = do
  let vec = stLegAmpRetrig st
  if i < 0 || i >= MV.length vec then pure False else MV.read vec i

setVibrato ∷ InstrumentId → Float → Float → AudioState → IO AudioState
setVibrato (InstrumentId i) rate depthC st = do
  let vr = stVibRateHz st
      vd = stVibDepthCents st
      rate'  = max 0 rate
      depth' = max 0 depthC
  if i < 0 || i >= MV.length vr
    then pure st
    else MV.write vr i rate' >> MV.write vd i depth' >> pure st

lookupVibrato ∷ InstrumentId → AudioState → IO (Float, Float)
lookupVibrato (InstrumentId i) st = do
  let vr = stVibRateHz st
      vd = stVibDepthCents st
  if i < 0 || i >= MV.length vr
    then pure (0,0)
    else (,) <$> MV.read vr i <*> MV.read vd i
