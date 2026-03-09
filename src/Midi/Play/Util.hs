{-# LANGUAGE Strict, UnicodeSyntax #-}

module Midi.Play.Util
  ( pushHeldNote
  , popHeldNote
  , enqueueDeferredRelease
  , takeDeferredReleases
  , ticksToMicroseconds
  ) where

import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

import Audio.Types (NoteInstanceId)

type HeldNotes = Map (Int, Int) [NoteInstanceId]
type DeferredReleases = Map Int [NoteInstanceId]

pushHeldNote ∷ Int → Int → NoteInstanceId → HeldNotes → HeldNotes
pushHeldNote ch key instId =
  M.insertWith (flip (++)) (ch, key) [instId]

popHeldNote ∷ Int → Int → HeldNotes → (Maybe NoteInstanceId, HeldNotes)
popHeldNote ch key held =
  case M.lookup (ch, key) held of
    Nothing -> (Nothing, held)
    Just [] -> (Nothing, M.delete (ch, key) held)
    Just (instId : rest) ->
      let held' =
            if null rest
              then M.delete (ch, key) held
              else M.insert (ch, key) rest held
      in (Just instId, held')

enqueueDeferredRelease ∷ Int → NoteInstanceId → DeferredReleases → DeferredReleases
enqueueDeferredRelease ch instId =
  M.insertWith (flip (++)) ch [instId]

takeDeferredReleases ∷ Int → DeferredReleases → ([NoteInstanceId], DeferredReleases)
takeDeferredReleases ch deferred =
  case M.lookup ch deferred of
    Nothing -> ([], deferred)
    Just instIds -> (instIds, M.delete ch deferred)

ticksToMicroseconds ∷ Int → Int → Int → Int
ticksToMicroseconds ppqn tempoUSPerQN dtTicks
  | ppqn <= 0 = error "ticksToMicroseconds: ppqn must be greater than 0"
  | tempoUSPerQN < 0 = error "ticksToMicroseconds: tempo must be non-negative"
  | dtTicks <= 0 = 0
  | micros > toInteger (maxBound ∷ Int) =
      error "ticksToMicroseconds: computed delay exceeds Int range"
  | otherwise = fromInteger micros
  where
    micros = (toInteger dtTicks * toInteger tempoUSPerQN) `div` toInteger ppqn
