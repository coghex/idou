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
  , MidiControls(..)
  , setChannelVolume
  , setChannelPan
  , setExpression
  , setModWheel
  , setPitchBend
  , resetMidiControls
  , lookupMidiControls
  ) where

import qualified Data.Vector.Mutable as MV

import Audio.Types (InstrumentId(..), Instrument(..), defaultPitchBendRangeSemitones)
import Audio.Thread.Types (AudioState(..))

data MidiControls = MidiControls
  { mcChannelVolume   ∷ !Float
  , mcExpression      ∷ !Float
  , mcChannelPan      ∷ !Float
  , mcModWheel        ∷ !Float
  , mcPitchBendSemis  ∷ !Float
  } deriving (Eq, Show)

clamp01 :: Float -> Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

clampSigned :: Float -> Float -> Float
clampSigned lim x
  | x < negate lim = negate lim
  | x > lim = lim
  | otherwise = x

defaultMidiControls :: MidiControls
defaultMidiControls =
  MidiControls
    { mcChannelVolume = 1
    , mcExpression = 1
    , mcChannelPan = 0
    , mcModWheel = 0
    , mcPitchBendSemis = 0
    }

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

setChannelVolume ∷ InstrumentId → Float → AudioState → IO AudioState
setChannelVolume (InstrumentId i) vol st = do
  let vec = stChannelVolume st
      vol' = clamp01 vol
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i vol' >> pure st

setChannelPan ∷ InstrumentId → Float → AudioState → IO AudioState
setChannelPan (InstrumentId i) pan st = do
  let vec = stChannelPan st
      pan' = clampSigned 1 pan
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i pan' >> pure st

setExpression ∷ InstrumentId → Float → AudioState → IO AudioState
setExpression (InstrumentId i) expr st = do
  let vec = stChannelExpression st
      expr' = clamp01 expr
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i expr' >> pure st

setModWheel ∷ InstrumentId → Float → AudioState → IO AudioState
setModWheel (InstrumentId i) mw st = do
  let vec = stModWheel st
      mw' = clamp01 mw
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i mw' >> pure st

setPitchBend ∷ InstrumentId → Float → AudioState → IO AudioState
setPitchBend (InstrumentId i) semis st = do
  let vec = stPitchBendSemis st
      semis' = clampSigned defaultPitchBendRangeSemitones semis
  if i < 0 || i >= MV.length vec then pure st else MV.write vec i semis' >> pure st

resetMidiControls ∷ InstrumentId → AudioState → IO AudioState
resetMidiControls iid st0 = do
  st1 <- setChannelVolume iid 1 st0
  st2 <- setExpression iid 1 st1
  st3 <- setChannelPan iid 0 st2
  st4 <- setModWheel iid 0 st3
  setPitchBend iid 0 st4

lookupMidiControls ∷ InstrumentId → AudioState → IO MidiControls
lookupMidiControls (InstrumentId i) st = do
  let vol = stChannelVolume st
      expr = stChannelExpression st
      pan = stChannelPan st
      mw = stModWheel st
      bend = stPitchBendSemis st
  if i < 0 || i >= MV.length vol
    then pure defaultMidiControls
    else do
      vol' <- MV.read vol i
      expr' <- MV.read expr i
      pan' <- MV.read pan i
      mw' <- MV.read mw i
      bend' <- MV.read bend i
      pure
        MidiControls
          { mcChannelVolume = vol'
          , mcExpression = expr'
          , mcChannelPan = pan'
          , mcModWheel = mw'
          , mcPitchBendSemis = bend'
          }
