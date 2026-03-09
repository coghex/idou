{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Midi.Play
  ( MidiChannelMap(..)
  , defaultChannelMap
  , playMidiFile
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Word (Word64)

import qualified Data.Map.Strict as M

import Audio.Patch (gmChannelInstrument, gmDrumInstrument, gmPercussionChannel)
import Audio.Thread (AudioSystem, sendAudio)
import Audio.Types
  ( AudioMsg(..)
  , InstrumentId(..)
  , NoteKey(..)
  , NoteInstanceId(..)
  )
import Midi.Play.Util (pushHeldNote, popHeldNote, ticksToMicroseconds)
import Midi.Control (controllerPanValue, controllerValue01, midiPitchBendSemitones, sustainPedalDown)

import Midi.ZMidi.Core.ReadFile (readMidi)
import Midi.ZMidi.Core.Datatypes
  ( MidiFile(..)
  , MidiHeader(..)
  , MidiTrack(..)
  , MidiTimeDivision(..)
  , MidiMessage
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
  | EvNoteAftertouch { evChan :: !Int, evKey :: !Int, evAftertouch :: !Int }
  | EvTempoUSPerQN { evTempo :: !Int }  -- microseconds per quarter note
  | EvController { evChan :: !Int, evController :: !Int, evValue :: !Int }
  | EvProgramChange { evChan :: !Int, evProgram :: !Int }
  | EvChanAftertouch { evChan :: !Int, evAftertouch :: !Int }
  | EvPitchBend { evChan :: !Int, evPitchBend :: !Int }
  deriving (Eq, Show)

data TimedEv = TimedEv
  { teTick :: !AbsTick
  , teEv   :: !MidiEv
  } deriving (Eq, Show)

type HeldNotes = Map (Int, Int) [NoteInstanceId]

type DeferredReleases = Map Int [NoteInstanceId]

type SustainChannels = Map Int Bool

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
      loop ppqn defaultTempoUSPerQN 0 M.empty M.empty M.empty (AbsTick 0) evs
  where
    defaultTempoUSPerQN :: Int
    defaultTempoUSPerQN = 500000  -- 120 bpm

    loop
      :: Int
      -> Int
      -> Word64
      -> HeldNotes
      -> DeferredReleases
      -> SustainChannels
      -> AbsTick
      -> [TimedEv]
      -> IO ()
    loop _ppqn _tempo _instCounter _held _deferred _sustain _prevTick [] = do
      -- release everything on the 16 MIDI channels
      forM_ [0..15] $ \ch ->
        sendAudio sys (AudioNoteOffInstrument (channelToInstrument chanMap ch))

    loop ppqn tempo instCounter held deferred sustain prevTick (TimedEv t ev : xs) = do
      let dt = unAbsTick t - unAbsTick prevTick
      when (dt > 0) $ threadDelay (ticksToMicroseconds ppqn tempo dt)

      case ev of
        EvTempoUSPerQN usPerQN ->
          loop ppqn usPerQN instCounter held deferred sustain t xs

        EvNoteOn ch key vel
          | vel <= 0 -> do
              (held', deferred') <- doNoteOff ch key held deferred sustain
              loop ppqn tempo instCounter held' deferred' sustain t xs

          | otherwise -> do
              let iid = channelToInstrument chanMap ch
                  velF = velToFloat vel
                  (instId, instCounter') = freshInstance instCounter

              when (ch == gmPercussionChannel) $
                sendAudio sys (AudioLoadInstrument iid (gmDrumInstrument key))

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

              let held' = pushHeldNote ch key instId held
              loop ppqn tempo instCounter' held' deferred sustain t xs

        EvNoteOff ch key -> do
          (held', deferred') <- doNoteOff ch key held deferred sustain
          loop ppqn tempo instCounter held' deferred' sustain t xs

        EvNoteAftertouch ch key pressure -> do
          let iid = channelToInstrument chanMap ch
              pressureF = controllerValue01 pressure
          case M.lookup (ch, key) held of
            Nothing -> pure ()
            Just instIds ->
              forM_ instIds $ \instId ->
                sendAudio sys (AudioSetNoteAftertouch iid instId pressureF)
          loop ppqn tempo instCounter held deferred sustain t xs

        EvController ch ctrl val -> do
          let iid = channelToInstrument chanMap ch
              wasSustainDown = M.findWithDefault False ch sustain
          case ctrl of
            1 -> do
              sendAudio sys (AudioSetModWheel iid (controllerValue01 val))
              loop ppqn tempo instCounter held deferred sustain t xs
            7 -> do
              sendAudio sys (AudioSetChannelVolume iid (controllerValue01 val))
              loop ppqn tempo instCounter held deferred sustain t xs
            10 -> do
              sendAudio sys (AudioSetChannelPan iid (controllerPanValue val))
              loop ppqn tempo instCounter held deferred sustain t xs
            11 -> do
              sendAudio sys (AudioSetExpression iid (controllerValue01 val))
              loop ppqn tempo instCounter held deferred sustain t xs
            64 ->
              if sustainPedalDown val
                then
                  loop ppqn tempo instCounter held deferred (M.insert ch True sustain) t xs
                else do
                  deferred' <- flushDeferred ch deferred
                  let sustain' = M.delete ch sustain
                  loop ppqn tempo instCounter held deferred' sustain' t xs
            120 -> do
              sendAudio sys (AudioNoteOffInstrument iid)
              let held' = dropHeldChannel ch held
                  deferred' = M.delete ch deferred
                  sustain' = M.delete ch sustain
              loop ppqn tempo instCounter held' deferred' sustain' t xs
            121 -> do
              sendAudio sys (AudioResetControllers iid)
              deferred' <-
                if wasSustainDown
                  then flushDeferred ch deferred
                  else pure deferred
              let sustain' = M.delete ch sustain
              loop ppqn tempo instCounter held deferred' sustain' t xs
            123 -> do
              sendAudio sys (AudioNoteOffInstrument iid)
              let held' = dropHeldChannel ch held
                  deferred' = M.delete ch deferred
                  sustain' = M.delete ch sustain
              loop ppqn tempo instCounter held' deferred' sustain' t xs
            _ ->
              loop ppqn tempo instCounter held deferred sustain t xs

        EvProgramChange ch program -> do
          let iid = channelToInstrument chanMap ch
          sendAudio sys (AudioLoadInstrument iid (gmChannelInstrument ch program))
          loop ppqn tempo instCounter held deferred sustain t xs

        EvChanAftertouch ch pressure -> do
          let iid = channelToInstrument chanMap ch
          sendAudio sys (AudioSetChannelAftertouch iid (controllerValue01 pressure))
          loop ppqn tempo instCounter held deferred sustain t xs

        EvPitchBend ch bend14 -> do
          let iid = channelToInstrument chanMap ch
          sendAudio sys (AudioSetPitchBend iid (midiPitchBendSemitones bend14))
          loop ppqn tempo instCounter held deferred sustain t xs

    doNoteOff
      :: Int -> Int
      -> HeldNotes
      -> DeferredReleases
      -> SustainChannels
      -> IO (HeldNotes, DeferredReleases)
    doNoteOff ch key held deferred sustainMap =
      let (mInstId, held') = popHeldNote ch key held
      in
        case mInstId of
          Nothing -> pure (held', deferred)
          Just instId -> do
            let sustainDown = M.findWithDefault False ch sustainMap
            if sustainDown
              then pure (held', M.insertWith (++) ch [instId] deferred)
              else do
                sendAudio sys $
                  AudioNoteOff
                    { instrumentId = channelToInstrument chanMap ch
                    , noteInstanceId = instId
                    }
                pure (held', deferred)

    flushDeferred :: Int -> DeferredReleases -> IO DeferredReleases
    flushDeferred ch deferred =
      case M.lookup ch deferred of
        Nothing -> pure deferred
        Just instIds -> do
          let iid = channelToInstrument chanMap ch
          forM_ instIds $ \instId ->
            sendAudio sys $
              AudioNoteOff
                { instrumentId = iid
                , noteInstanceId = instId
                }
          pure (M.delete ch deferred)

    dropHeldChannel :: Int -> HeldNotes -> HeldNotes
    dropHeldChannel ch = M.filterWithKey (\(ch', _) _ -> ch' /= ch)

--------------------------------------------------------------------------------
-- Conversion from zmidi-core CST to timed events

midiPPQN :: MidiFile -> Int
midiPPQN mf =
  case time_division (mf_header mf) of
    TPB w16 ->
      let ppqn = fromIntegral w16
      in
        if ppqn > 0
          then ppqn
          else error "Invalid MIDI header: PPQN must be greater than 0."
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
        NoteAftertouch ch key pressure ->
          [TimedEv t (EvNoteAftertouch (fromIntegral ch) (fromIntegral key) (fromIntegral pressure))]
        Controller ch ctrl val ->
          [TimedEv t (EvController (fromIntegral ch) (fromIntegral ctrl) (fromIntegral val))]
        ProgramChange ch program ->
          [TimedEv t (EvProgramChange (fromIntegral ch) (fromIntegral program))]
        ChanAftertouch ch pressure ->
          [TimedEv t (EvChanAftertouch (fromIntegral ch) (fromIntegral pressure))]
        PitchBend ch bend ->
          [TimedEv t (EvPitchBend (fromIntegral ch) (fromIntegral bend))]

    _ -> []

--------------------------------------------------------------------------------
-- Utilities

velToFloat :: Int -> Float
velToFloat v =
  let x = max 0 (min 127 v)
  in fromIntegral x / 127

freshInstance :: Word64 -> (NoteInstanceId, Word64)
freshInstance n =
  let n' = n + 1
  in (NoteInstanceId n', n')
