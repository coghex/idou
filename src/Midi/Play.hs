{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Midi.Play
  ( MidiChannelMap(..)
  , defaultChannelMap
  , playMidiFile
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Word (Word16, Word32, Word64, Word8)
import System.IO (FilePath)

import qualified Data.Map.Strict as M

import Audio.Thread (AudioSystem, sendAudio)
import Audio.Types
  ( AudioMsg(..)
  , InstrumentId(..)
  , NoteKey(..)
  , NoteInstanceId(..)
  )

import Midi.ZMidi.Core.ReadFile (readMidi)
import Midi.ZMidi.Core.Datatypes
  ( MidiFile(..)
  , MidiHeader(..)
  , MidiTrack(..)
  , MidiTimeDivision(..)
  , MidiMessage
  , DeltaTime(..)
  , MidiEvent(..)
  , MidiVoiceEvent(..)
  , MidiMetaEvent(..)
  )

--------------------------------------------------------------------------------
-- Public configuration

newtype MidiChannelMap = MidiChannelMap
  { channelToInstrument ∷ Int -> InstrumentId
  }

defaultChannelMap :: MidiChannelMap
defaultChannelMap = MidiChannelMap InstrumentId

--------------------------------------------------------------------------------
-- Internal event representation

newtype AbsTick = AbsTick { unAbsTick :: Int }
  deriving (Eq, Ord, Show)

data MidiEv
  = EvNoteOn  { evChan :: !Int, evKey :: !Int, evVel :: !Int }
  | EvNoteOff { evChan :: !Int, evKey :: !Int }
  | EvTempoUSPerQN { evTempo :: !Int }  -- microseconds per quarter note
  deriving (Eq, Show)

data TimedEv = TimedEv
  { teTick :: !AbsTick
  , teEv   :: !MidiEv
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Top-level

playMidiFile :: MidiChannelMap -> AudioSystem -> FilePath -> IO ()
playMidiFile chanMap sys fp = do
  e <- readMidi fp
  case e of
    Left perr -> error ("readMidi failed: " <> show perr)
    Right mf  -> do
      let ppqn = midiPPQN mf
          evs  = sortOn (unAbsTick . teTick) (midiToTimedEvents mf)
      loop ppqn defaultTempoUSPerQN 0 M.empty (AbsTick 0) evs
  where
    defaultTempoUSPerQN :: Int
    defaultTempoUSPerQN = 500000  -- 120 bpm

    loop
      :: Int
      -> Int
      -> Word64
      -> Map (Int, Int) [NoteInstanceId]
      -> AbsTick
      -> [TimedEv]
      -> IO ()
    loop _ppqn _tempo _instCounter _held _prevTick [] = do
      -- release everything on the 16 MIDI channels
      forM_ [0..15] $ \ch ->
        sendAudio sys (AudioNoteOffInstrument (channelToInstrument chanMap ch))

    loop ppqn tempo instCounter held prevTick (TimedEv t ev : xs) = do
      let dt = unAbsTick t - unAbsTick prevTick
      when (dt > 0) $ threadDelay (ticksToMicroseconds ppqn tempo dt)

      case ev of
        EvTempoUSPerQN usPerQN ->
          loop ppqn usPerQN instCounter held t xs

        EvNoteOn ch key vel
          | vel <= 0 -> do
              (held', instCounter') <- doNoteOff ch key held instCounter
              loop ppqn tempo instCounter' held' t xs

          | otherwise -> do
              let iid = channelToInstrument chanMap ch
                  velF = velToFloat vel
                  (instId, instCounter') = freshInstance instCounter

              sendAudio sys $
                AudioNoteOn
                  { instrumentId = iid
                  , amp = 1.0
                  , pan = 0.0
                  , noteKey = NoteKey key
                  , noteInstanceId = instId
                  , velocity = velF
                  , adsrOverride = Nothing
                  }

              let held' = M.insertWith (++) (ch, key) [instId] held
              loop ppqn tempo instCounter' held' t xs

        EvNoteOff ch key -> do
          (held', instCounter') <- doNoteOff ch key held instCounter
          loop ppqn tempo instCounter' held' t xs

    -- Note: we keep a stack so repeated NoteOns of same (chan,key)
    -- will be released LIFO, which matches typical MIDI sequencing.
    doNoteOff
      :: Int -> Int
      -> Map (Int, Int) [NoteInstanceId]
      -> Word64
      -> IO (Map (Int, Int) [NoteInstanceId], Word64)
    doNoteOff ch key held instCounter =
      case M.lookup (ch, key) held of
        Nothing -> pure (held, instCounter)
        Just [] -> pure (M.delete (ch, key) held, instCounter)
        Just (instId:rest) -> do
          let iid = channelToInstrument chanMap ch
          sendAudio sys $
            AudioNoteOff
              { instrumentId = iid
              , noteInstanceId = instId
              }
          let held' =
                if null rest
                  then M.delete (ch, key) held
                  else M.insert (ch, key) rest held
          pure (held', instCounter)

--------------------------------------------------------------------------------
-- Conversion from zmidi-core CST to timed events

midiPPQN :: MidiFile -> Int
midiPPQN mf =
  case time_division (mf_header mf) of
    TPB w16 -> fromIntegral w16
    FPS _   ->
      error "SMPTE time division (FPS) not supported yet; add SMPTE support or convert the MIDI."

midiToTimedEvents :: MidiFile -> [TimedEv]
midiToTimedEvents mf =
  concatMap trackToTimed (mf_tracks mf)

trackToTimed :: MidiTrack -> [TimedEv]
trackToTimed (MidiTrack msgs) =
  snd $ foldl step (AbsTick 0, []) msgs
  where
    step :: (AbsTick, [TimedEv]) -> MidiMessage -> (AbsTick, [TimedEv])
    step (AbsTick acc, out) (dt, ev) =
      let acc' = acc + fromIntegral dt
          t    = AbsTick acc'
      in (t, out <> extractEvent t ev)

extractEvent :: AbsTick -> MidiEvent -> [TimedEv]
extractEvent t ev =
  case ev of
    MetaEvent me ->
      case me of
        SetTempo usPerQN -> [TimedEv t (EvTempoUSPerQN (fromIntegral usPerQN))]
        _                -> []

    VoiceEvent _rs ve ->
      case ve of
        NoteOn ch key vel ->
          [TimedEv t (EvNoteOn (fromIntegral ch) (fromIntegral key) (fromIntegral vel))]
        NoteOff ch key _relVel ->
          [TimedEv t (EvNoteOff (fromIntegral ch) (fromIntegral key))]
        _ ->
          []

    _ -> []

--------------------------------------------------------------------------------
-- Utilities

ticksToMicroseconds :: Int -> Int -> Int -> Int
ticksToMicroseconds ppqn tempoUSPerQN dtTicks =
  fromIntegral
    ((fromIntegral dtTicks :: Int64) * fromIntegral tempoUSPerQN `div` fromIntegral ppqn)

velToFloat :: Int -> Float
velToFloat v =
  let x = max 0 (min 127 v)
  in fromIntegral x / 127

freshInstance :: Word64 -> (NoteInstanceId, Word64)
freshInstance n =
  let n' = n + 1
  in (NoteInstanceId n', n')
