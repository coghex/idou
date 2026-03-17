{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Instrument
  ( lookupInstrumentSpec
  , loadResolvedInstrument
  , usesBuiltInDrumPatch
  ) where

import Data.List (find)

import Audio.Patch (gmProgramInstrument)
import Audio.Patch.Loader (loadInstrumentPatch)
import Audio.Types (Instrument(..), InstrumentId(..))
import Player.Timeline (InstrumentPatternSpec(..))

lookupInstrumentSpec ∷ InstrumentId → [InstrumentPatternSpec] → Maybe InstrumentPatternSpec
lookupInstrumentSpec iid = find ((== iid) . ipInstrumentId)

loadResolvedInstrument ∷ InstrumentPatternSpec → IO Instrument
loadResolvedInstrument spec =
  case ipPatchPath spec of
    Just path ->
      loadInstrumentPatch path
    Nothing ->
      let InstrumentId program = ipInstrumentId spec
      in pure (gmProgramInstrument program)

usesBuiltInDrumPatch ∷ InstrumentPatternSpec → Bool
usesBuiltInDrumPatch spec =
  case (ipInstrumentId spec, ipPatchPath spec) of
    (InstrumentId 9, Nothing) -> True
    _ -> False
