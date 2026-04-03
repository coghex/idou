{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Midi.Play
  ( MidiChannelMap(..)
  , MidiPlaybackPlan
  , defaultChannelMap
  , loadMidiPlaybackPlan
  , playMidiPlaybackPlan
  , playMidiFile
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Word (Word64)

import qualified Data.Map.Strict as M

import Audio.Patch (gmChannelInstrument, gmDrumInstrument, gmPercussionChannel)
import Audio.Thread (AudioSystem, sendAudio)
import Audio.Types
  ( AudioBus(..)
  , AudioMsg(..)
  , InstrumentId(..)
  , NoteKey(..)
  , NoteInstanceId(..)
  )
import Midi.Play.Util
  ( pushHeldNote
  , popHeldNote
  , enqueueDeferredRelease
  , takeDeferredReleases
  , ticksToMicrosecondsEither
  )
import Midi.Control (controllerPanValue, controllerValue01, midiPitchBendSemitones, sustainPedalDown)

import Midi.ZMidi.Core.ReadFile (ParseErr, readMidi)
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

data MidiPlaybackPlan = MidiPlaybackPlan
  { mppPpqn :: !Int
  , mppEvents :: ![TimedEv]
  }

--------------------------------------------------------------------------------
-- Top-level

loadMidiPlaybackPlan ∷ FilePath → IO (Either String MidiPlaybackPlan)
loadMidiPlaybackPlan fp = do
  readResult <- try (readMidi fp) ∷ IO (Either SomeException (Either ParseErr MidiFile))
  pure $
    case readResult of
      Left ex ->
        Left ("Failed to read MIDI file " <> fp <> ": " <> displayException ex)
      Right e ->
        case e of
          Left perr ->
            Left ("Failed to read MIDI file " <> fp <> ": " <> show perr)
          Right mf ->
            case midiPPQNE mf of
              Left err ->
                Left ("Failed to prepare MIDI file " <> fp <> ": " <> err)
              Right ppqn ->
                Right MidiPlaybackPlan
                  { mppPpqn = ppqn
                  , mppEvents = sortOn (unAbsTick . teTick) (midiToTimedEvents mf)
                  }

playMidiFile ∷ MidiChannelMap → AudioSystem → FilePath → IO (Either String ())
playMidiFile chanMap sys fp = do
  planResult <- loadMidiPlaybackPlan fp
  case planResult of
    Left err -> pure (Left err)
    Right plan -> playMidiPlaybackPlan chanMap sys plan

playMidiPlaybackPlan ∷ MidiChannelMap → AudioSystem → MidiPlaybackPlan → IO (Either String ())
playMidiPlaybackPlan chanMap sys plan =
  loop (mppPpqn plan) defaultTempoUSPerQN 0 M.empty M.empty M.empty (AbsTick 0) (mppEvents plan)
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
      -> IO (Either String ())
    loop _ppqn _tempo _instCounter _held _deferred _sustain _prevTick [] = do
      releaseAllChannels
      pure (Right ())

    loop ppqn tempo instCounter held deferred sustain prevTick (TimedEv t ev : xs) = do
      let dt = unAbsTick t - unAbsTick prevTick
      delayResult <-
        if dt > 0
          then
            case ticksToMicrosecondsEither ppqn tempo dt of
              Left err -> failPlayback ("MIDI playback timing failed: " <> err)
              Right micros -> threadDelay micros >> pure (Right ())
          else pure (Right ())

      case delayResult of
        Left err -> pure (Left err)
        Right () ->
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
                      , noteBus = AudioBusMusic
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
              then pure (held', enqueueDeferredRelease ch instId deferred)
              else do
                sendAudio sys $
                  AudioNoteOff
                    { instrumentId = channelToInstrument chanMap ch
                    , noteInstanceId = instId
                    }
                pure (held', deferred)

    flushDeferred :: Int -> DeferredReleases -> IO DeferredReleases
    flushDeferred ch deferred =
      let (instIds, deferred') = takeDeferredReleases ch deferred
      in
        if null instIds
          then pure deferred'
          else do
            let iid = channelToInstrument chanMap ch
            forM_ instIds $ \instId ->
              sendAudio sys $
                AudioNoteOff
                  { instrumentId = iid
                  , noteInstanceId = instId
                  }
            pure deferred'

    dropHeldChannel :: Int -> HeldNotes -> HeldNotes
    dropHeldChannel ch = M.filterWithKey (\(ch', _) _ -> ch' /= ch)

    releaseAllChannels :: IO ()
    releaseAllChannels =
      forM_ [0..15] $ \ch ->
        sendAudio sys (AudioNoteOffInstrument (channelToInstrument chanMap ch))

    failPlayback :: String -> IO (Either String ())
    failPlayback message = do
      releaseAllChannels
      pure (Left message)

--------------------------------------------------------------------------------
-- Conversion from zmidi-core CST to timed events

midiPPQNE ∷ MidiFile → Either String Int
midiPPQNE mf =
  case time_division (mf_header mf) of
    TPB w16 ->
      let ppqn = fromIntegral w16
      in
        if ppqn > 0
          then Right ppqn
          else Left "Invalid MIDI header: PPQN must be greater than 0."
    FPS _   ->
      Left "SMPTE time division (FPS) is not supported yet; convert the MIDI to PPQN timing first."

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
