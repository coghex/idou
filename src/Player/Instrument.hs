{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Instrument
  ( lookupInstrumentSpec
  , loadResolvedInstrumentEither
  , loadResolvedInstrument
  , usesBuiltInDrumPatch
  ) where

import Data.List (find)

import Audio.Patch (gmProgramInstrument)
import Audio.Patch.Loader (loadInstrumentPatch, loadInstrumentPatchEither)
import Audio.Types (Instrument(..), InstrumentId(..))
import Player.Timeline (InstrumentPatternSpec(..))

lookupInstrumentSpec ∷ InstrumentId → [InstrumentPatternSpec] → Maybe InstrumentPatternSpec
lookupInstrumentSpec iid = find ((== iid) . ipInstrumentId)

loadResolvedInstrumentEither ∷ InstrumentPatternSpec → IO (Either String Instrument)
loadResolvedInstrumentEither spec =
  case ipPatchPath spec of
    Just path ->
      loadInstrumentPatchEither path
    Nothing ->
      let InstrumentId program = ipInstrumentId spec
      in pure (Right (gmProgramInstrument program))

loadResolvedInstrument ∷ InstrumentPatternSpec → IO Instrument
loadResolvedInstrument spec = do
  result <- loadResolvedInstrumentEither spec
  case result of
    Left err -> ioError (userError err)
    Right inst -> pure inst

usesBuiltInDrumPatch ∷ InstrumentPatternSpec → Bool
usesBuiltInDrumPatch spec =
  case (ipInstrumentId spec, ipPatchPath spec) of
    (InstrumentId 9, Nothing) -> True
    _ -> False
