{-# LANGUAGE Strict, UnicodeSyntax #-}

module Midi.Control
  ( controllerValue01
  , controllerPanValue
  , sustainPedalDown
  , midiPitchBendSemitones
  ) where

import Audio.Types (defaultPitchBendRangeSemitones)

controllerValue01 ∷ Int → Float
controllerValue01 v =
  let x = max 0 (min 127 v)
  in fromIntegral x / 127

controllerPanValue ∷ Int → Float
controllerPanValue v =
  let x = max 0 (min 127 v)
  in
    if x == 64
      then 0
      else
        if x < 64
          then fromIntegral (x - 64) / 64
          else fromIntegral (x - 64) / 63

sustainPedalDown ∷ Int → Bool
sustainPedalDown v = v >= 64

midiPitchBendSemitones ∷ Int → Float
midiPitchBendSemitones raw =
  let x = max 0 (min 16383 raw)
      normalized
        | x >= 8192 = fromIntegral (x - 8192) / 8191
        | otherwise = fromIntegral (x - 8192) / 8192
  in normalized * defaultPitchBendRangeSemitones
