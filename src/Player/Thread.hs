{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Thread
  ( AutomationCurve(..)
  , PlayerMsg(..)
  , PlayerEvent(..)
  , PlayerSystem(..)
  , nextBarFrame
  , startPlayerThread
  , stopPlayerThread
  , sendPlayer
  , tryReadPlayerEvent
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (foldM)
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)

import Audio.Thread
  ( AudioSystem(..)
  , readTransportFrame
  , scheduleAudioActionAtFrame
  , sendAudio
  , tryReadAudioEvent
  )
import Audio.Types
  ( AudioBus
  , AudioEvent
  , AudioMsg(..)
  , ClipId
  , InstrumentId(..)
  , NoteInstanceId(..)
  , NoteKey(..)
  , ScheduledAudioAction(..)
  , sampleRate
  )
import Audio.Envelope (ADSR)
import Audio.Patch (gmDrumInstrument)
import Engine.Core.Queue (Queue)
import qualified Engine.Core.Queue as Q
import Engine.Core.Thread (ThreadControl(..), ThreadState(..))
import Player.Automation
  ( AutomationCurve(..)
  , EnergyLane(..)
  , EnergyLaneEvent(..)
  , MoodLane(..)
  , MoodLaneEvent(..)
  , barFramesForTransport
  , scheduleEnergyLaneNextBar
  , scheduleMoodLaneNextBar
  , stepEnergyLane
  , stepMoodLane
  )
import Player.Timeline
  ( SongSpec
  , TimelineBar(..)
  , TimelineNote(..)
  , TimelineTransitionTelemetry
  , TimelineRuntime(..)
  , compileTimelineBars
  , loadSongSpec
  , popTransitionTelemetry
  , popReadyBars
    , prepareTimelineRuntime
    , setTimelineGenre
    , setTimelineTargets
    , sgGenre
    , timelineBoundarySpanFromNextBar
    , timelineNextBarBoundaryFrame
    , timelineRuntimeDone
    , timelineRuntimeEndFrame
    , validateSongGenre
    )

data PlayerMsg
  = PlayerShutdown
  | PlayerPause
  | PlayerResume
  | PlayerSetMeter
      { pmBeatsPerBar ∷ !Int
      }
  | PlayerSetTempoBpm
      { pmTempoBpm ∷ !Float
      }
  | PlayerSetMoodTarget
      { pmMoodTarget ∷ !String
      }
  | PlayerSetGenreTarget
      { pmGenreTarget ∷ !String
      }
  | PlayerClearGenreTarget
  | PlayerClearMoodTarget
  | PlayerSetEnergyTarget
      { pmEnergyTarget ∷ !Float
      }
  | PlayerAutomateEnergyNextBar
      { pmAutoEnergyTarget ∷ !Float
      , pmAutoEnergyBars ∷ !Int
      , pmAutoEnergyCurve ∷ !AutomationCurve
      }
  | PlayerAutomateMoodNextBar
      { pmAutoMoodTarget ∷ !String
      , pmAutoMoodBars ∷ !Int
      }
  | PlayerCancelEnergyAutomation
  | PlayerCancelMoodAutomation
  | PlayerLoadSongTimeline
      { pmSongPath ∷ !FilePath
      }
  | PlayerStartSongTimeline
  | PlayerStopSongTimeline
  | PlayerScheduleClipNextBar
      { pmClipId ∷ !ClipId
      , pmClipBus ∷ !AudioBus
      , pmClipGain ∷ !Float
      , pmClipPan ∷ !Float
      , pmClipLoop ∷ !Bool
      }
  | PlayerScheduleBusGainNextBar
      { pmGainBus ∷ !AudioBus
      , pmGainValue ∷ !Float
      }
  | PlayerScheduleNoteOnNextBar
      { pmNoteOnInstrumentId ∷ !InstrumentId
      , pmNoteOnAmp ∷ !Float
      , pmNoteOnPan ∷ !Float
      , pmNoteOnKey ∷ !NoteKey
      , pmNoteOnInstanceId ∷ !NoteInstanceId
      , pmNoteOnVelocity ∷ !Float
      , pmNoteOnAdsrOverride ∷ !(Maybe ADSR)
      }
  | PlayerScheduleNoteOffNextBar
      { pmNoteOffInstrumentId ∷ !InstrumentId
      , pmNoteOffInstanceId ∷ !NoteInstanceId
      }
  deriving (Eq, Show)

data PlayerEvent
  = PlayerEventAudio !AudioEvent
  | PlayerEventTimelineLoaded !FilePath
  | PlayerEventTimelineLoadFailed !FilePath !String
  | PlayerEventTimelineStarted
      { peTimelineStartFrame ∷ !Word64
      , peTimelineBarCount   ∷ !Int
      }
  | PlayerEventTimelineStopped
  | PlayerEventTimelineFinished
  | PlayerEventTimelineTransition !TimelineTransitionTelemetry
  | PlayerEventEnergyAutomationStarted
      { peEnergyAutoStartFrame ∷ !Word64
      , peEnergyAutoEndFrame ∷ !Word64
      , peEnergyAutoFrom ∷ !Float
      , peEnergyAutoTo ∷ !Float
      , peEnergyAutoCurve ∷ !AutomationCurve
      }
  | PlayerEventEnergyAutomationCompleted
      { peEnergyAutoEndFrame ∷ !Word64
      , peEnergyAutoFinal ∷ !Float
      }
  | PlayerEventMoodAutomationStarted
      { peMoodAutoStartFrame ∷ !Word64
      , peMoodAutoEndFrame ∷ !Word64
      , peMoodAutoFrom ∷ !(Maybe String)
      , peMoodAutoTo ∷ !(Maybe String)
      }
  | PlayerEventMoodAutomationCompleted
      { peMoodAutoEndFrame ∷ !Word64
      , peMoodAutoFinal ∷ !(Maybe String)
      }
  | PlayerEventEnergyAutomationCanceled
  | PlayerEventMoodAutomationCanceled
  | PlayerEventGenreChanged
      { peGenreCurrent ∷ !String
      }
  | PlayerEventGenreChangeRejected
      { peGenreRequested ∷ !String
      , peGenreError     ∷ !String
      }
  | PlayerEventScheduled
      { peFrame ∷ !Word64
      , peAction ∷ !ScheduledAudioAction
      }
  deriving (Eq, Show)

data PlayerState = PlayerState
  { pstBeatsPerBar ∷ !Int
  , pstTempoBpm    ∷ !Float
  , pstGenreTarget ∷ !(Maybe String)
  , pstMoodTarget  ∷ !(Maybe String)
  , pstEnergyTarget ∷ !Float
  , pstEnergyLane  ∷ !(Maybe EnergyLane)
  , pstMoodLane    ∷ !(Maybe MoodLane)
  , pstSongSpec    ∷ !(Maybe SongSpec)
  , pstTimeline    ∷ !(Maybe TimelineRuntime)
  , pstNextNoteInst ∷ !Word64
  }

data PlayerSystem = PlayerSystem
  { psMsgQueue   ∷ !(Queue PlayerMsg)
  , psEventQueue ∷ !(Queue PlayerEvent)
  , psThread     ∷ !ThreadState
  }

startPlayerThread ∷ AudioSystem → IO PlayerSystem
startPlayerThread audioSys = do
  msgQ <- Q.newQueue
  evQ <- Q.newQueue
  controlRef <- newIORef ThreadRunning
  playerStateRef <- newIORef (PlayerState 4 120 Nothing Nothing 0.5 Nothing Nothing Nothing Nothing 1)
  doneVar <- newEmptyMVar
  tid <- forkIO $
    runPlayerLoop audioSys msgQ evQ controlRef playerStateRef `finally` putMVar doneVar ()
  pure
    PlayerSystem
      { psMsgQueue = msgQ
      , psEventQueue = evQ
      , psThread = ThreadState controlRef tid doneVar
      }

stopPlayerThread ∷ PlayerSystem → IO ()
stopPlayerThread ps = do
  Q.writeQueue (psMsgQueue ps) PlayerShutdown
  takeMVar (tsDone (psThread ps))

sendPlayer ∷ PlayerSystem → PlayerMsg → IO ()
sendPlayer ps msg = Q.writeQueue (psMsgQueue ps) msg

tryReadPlayerEvent ∷ PlayerSystem → IO (Maybe PlayerEvent)
tryReadPlayerEvent ps = Q.tryReadQueue (psEventQueue ps)

runPlayerLoop
  ∷ AudioSystem
  → Queue PlayerMsg
  → Queue PlayerEvent
  → IORef ThreadControl
  → IORef PlayerState
  → IO ()
runPlayerLoop audioSys msgQ evQ controlRef playerStateRef = loop
  where
    loop = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> do
          processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef
          drainAudioEvents audioSys evQ
        ThreadPaused -> do
          processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef
          threadDelay 10000
          loop
        ThreadRunning -> do
          processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef
          pumpTimeline audioSys evQ playerStateRef
          drainAudioEvents audioSys evQ
          threadDelay 1000
          loop

processPlayerMsgs
  ∷ AudioSystem
  → IORef ThreadControl
  → Queue PlayerMsg
  → Queue PlayerEvent
  → IORef PlayerState
  → IO ()
processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef = do
  m <- Q.tryReadQueue msgQ
  case m of
    Nothing -> pure ()
    Just msg -> do
      case msg of
        PlayerShutdown -> writeIORef controlRef ThreadStopped
        PlayerPause -> writeIORef controlRef ThreadPaused
        PlayerResume -> writeIORef controlRef ThreadRunning
        PlayerSetMeter beats ->
          modifyIORef' playerStateRef (\s -> s { pstBeatsPerBar = max 1 beats })
        PlayerSetTempoBpm bpm -> do
          let bpm' = if isNaN bpm || isInfinite bpm then 120 else max 1 bpm
          modifyIORef' playerStateRef (\s -> s { pstTempoBpm = bpm' })
          sendAudio audioSys (AudioSetTransportBpm bpm')
        PlayerSetGenreTarget genreRaw -> do
          ps <- readIORef playerStateRef
          let genre' = normalizeGenreTarget genreRaw
          case validateGenreTarget ps genre' of
            Left err ->
              Q.writeQueue evQ (PlayerEventGenreChangeRejected genreRaw err)
            Right currentGenre -> do
              writeIORef playerStateRef (applyLiveTimelineSettings (ps { pstGenreTarget = genre' }))
              Q.writeQueue evQ (PlayerEventGenreChanged currentGenre)
        PlayerClearGenreTarget -> do
          ps <- readIORef playerStateRef
          case validateGenreTarget ps Nothing of
            Left err ->
              Q.writeQueue evQ (PlayerEventGenreChangeRejected "<default>" err)
            Right currentGenre -> do
              writeIORef playerStateRef (applyLiveTimelineSettings (ps { pstGenreTarget = Nothing }))
              Q.writeQueue evQ (PlayerEventGenreChanged currentGenre)
        PlayerSetMoodTarget moodRaw -> do
          let mood' = normalizeMoodTarget moodRaw
          modifyIORef'
            playerStateRef
            (\s -> applyLiveTimelineSettings (s { pstMoodTarget = mood', pstMoodLane = Nothing }))
        PlayerClearMoodTarget ->
          modifyIORef'
            playerStateRef
            (\s -> applyLiveTimelineSettings (s { pstMoodTarget = Nothing, pstMoodLane = Nothing }))
        PlayerSetEnergyTarget energy ->
          let energy' = clamp01 energy
          in modifyIORef'
               playerStateRef
               (\s -> applyLiveTimelineSettings (s { pstEnergyTarget = energy', pstEnergyLane = Nothing }))
        PlayerAutomateEnergyNextBar target bars curve -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let (lane, ps') = mkEnergyLane audioSys now target bars curve ps
          writeIORef playerStateRef ps' { pstEnergyLane = Just lane }
        PlayerAutomateMoodNextBar moodRaw bars -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let target = normalizeMoodTarget moodRaw
              (lane, ps') = mkMoodLane audioSys now target bars ps
          writeIORef playerStateRef ps' { pstMoodLane = Just lane }
        PlayerCancelEnergyAutomation -> do
          ps <- readIORef playerStateRef
          modifyIORef' playerStateRef (\s -> s { pstEnergyLane = Nothing })
          case pstEnergyLane ps of
            Nothing -> pure ()
            Just _ -> Q.writeQueue evQ PlayerEventEnergyAutomationCanceled
        PlayerCancelMoodAutomation -> do
          ps <- readIORef playerStateRef
          modifyIORef' playerStateRef (\s -> s { pstMoodLane = Nothing })
          case pstMoodLane ps of
            Nothing -> pure ()
            Just _ -> Q.writeQueue evQ PlayerEventMoodAutomationCanceled
        PlayerLoadSongTimeline path -> do
          loaded <- loadSongSpec path
          case loaded of
            Left err ->
              Q.writeQueue evQ (PlayerEventTimelineLoadFailed path err)
            Right spec -> do
              let bars = compileTimelineBars (sampleRate (asHandle audioSys)) spec
              case bars of
                [] -> pure ()
                firstBar : _ -> do
                  modifyIORef'
                    playerStateRef
                    ( \s ->
                        s
                          { pstBeatsPerBar = tbBeatsPerBar firstBar
                          , pstTempoBpm = tbTempoBpm firstBar
                          }
                    )
                  sendAudio audioSys (AudioSetTransportBpm (tbTempoBpm firstBar))
              modifyIORef'
                playerStateRef
                (\s -> s { pstSongSpec = Just spec, pstGenreTarget = Nothing, pstTimeline = Nothing })
              Q.writeQueue evQ (PlayerEventTimelineLoaded path)
        PlayerStartSongTimeline -> do
          ps <- readIORef playerStateRef
          case pstSongSpec ps of
            Nothing ->
              Q.writeQueue
                evQ
                (PlayerEventTimelineLoadFailed "<runtime>" "no song loaded")
            Just spec -> do
              now <- readTransportFrame audioSys
              let frame =
                    nextBarFrame
                      now
                      (sampleRate (asHandle audioSys))
                      (pstBeatsPerBar ps)
                      (pstTempoBpm ps)
                  runtime =
                    applyLiveTimelineRuntime
                      ps
                      ( prepareTimelineRuntime
                          (sampleRate (asHandle audioSys))
                          frame
                          spec
                      )
                  estimatedBars = max 1 (length (compileTimelineBars (sampleRate (asHandle audioSys)) spec))
              modifyIORef' playerStateRef (\s -> s { pstTimeline = Just runtime })
              Q.writeQueue evQ (PlayerEventTimelineStarted frame estimatedBars)
        PlayerStopSongTimeline -> do
          modifyIORef' playerStateRef (\s -> s { pstTimeline = Nothing })
          sendAudio audioSys AudioStopAll
          Q.writeQueue evQ PlayerEventTimelineStopped
        PlayerScheduleClipNextBar cid bus gain pan loop -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let (frame, ps') = resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action =
                ScheduledPlayClip
                  { saClipId = cid
                  , saClipBus = bus
                  , saClipGain = gain
                  , saClipPan = pan
                  , saClipLoop = loop
                  }
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
        PlayerScheduleBusGainNextBar bus gain -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let (frame, ps') = resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action = ScheduledSetBusGain bus gain
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
        PlayerScheduleNoteOnNextBar iid amp pan key noteInst vel adsrOverride -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let (frame, ps') = resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action =
                ScheduledNoteOn
                  { saInstrumentId = iid
                  , saAmp = amp
                  , saPan = pan
                  , saNoteKey = key
                  , saNoteInstanceId = noteInst
                  , saVelocity = vel
                  , saAdsrOverride = adsrOverride
                  }
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
        PlayerScheduleNoteOffNextBar iid noteInst -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let (frame, ps') = resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action =
                ScheduledNoteOff
                  { saInstrumentId = iid
                  , saNoteInstanceId = noteInst
                  }
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
      processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef

applyLiveTimelineSettings ∷ PlayerState → PlayerState
applyLiveTimelineSettings ps =
  case pstTimeline ps of
    Nothing -> ps
    Just timeline ->
      ps
        { pstTimeline =
            Just
              (applyLiveTimelineRuntime ps timeline)
        }

applyLiveTimelineRuntime ∷ PlayerState → TimelineRuntime → TimelineRuntime
applyLiveTimelineRuntime ps timeline =
  let timeline' =
        setTimelineTargets
          (pstMoodTarget ps)
          (pstEnergyTarget ps)
          timeline
  in case activeGenreTarget ps of
       Nothing -> timeline'
       Just genre ->
         case setTimelineGenre genre timeline' of
           Left _ -> timeline'
           Right timeline'' -> timeline''

normalizeMoodTarget ∷ String → Maybe String
normalizeMoodTarget raw =
  let m = map toLower (trim raw)
  in if null m then Nothing else Just m

normalizeGenreTarget ∷ String → Maybe String
normalizeGenreTarget raw =
  let g = map toLower (trim raw)
  in if null g || g == "none" || g == "default"
       then Nothing
       else Just g

activeGenreTarget ∷ PlayerState → Maybe String
activeGenreTarget ps =
  case pstGenreTarget ps of
    Just genre -> Just genre
    Nothing -> baseSongGenre ps

baseSongGenre ∷ PlayerState → Maybe String
baseSongGenre ps =
  case pstSongSpec ps of
    Nothing -> Nothing
    Just spec ->
      let genre = trim (sgGenre spec)
      in if null genre then Nothing else Just genre

validateGenreTarget ∷ PlayerState → Maybe String → Either String String
validateGenreTarget ps target =
  let resolved =
        case target of
          Just genre -> genre
          Nothing -> fromMaybe "" (baseSongGenre ps)
  in if null resolved
       then Left "live genre switching requires a song using song.genre"
       else validateSongGenre resolved

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse

clamp01 ∷ Float → Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

pumpTimeline
  ∷ AudioSystem
  → Queue PlayerEvent
  → IORef PlayerState
  → IO ()
pumpTimeline audioSys evQ playerStateRef = do
  now <- readTransportFrame audioSys
  ps0 <- readIORef playerStateRef
  ps <- advanceAutomationLanes now evQ ps0
  writeIORef playerStateRef ps
  case pstTimeline ps of
    Nothing -> pure ()
    Just runtime -> do
      let (readyBars, runtime') = popReadyBars now runtime
          (transitionTelemetry, runtime'') = popTransitionTelemetry runtime'
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineTransition) transitionTelemetry
      ps' <- foldM (scheduleTimelineBar audioSys evQ runtime'') ps readyBars
      if timelineRuntimeDone runtime''
        then
          case timelineRuntimeEndFrame runtime'' of
            Just endFrame
              | now < endFrame ->
                  writeIORef playerStateRef ps' { pstTimeline = Just runtime'' }
            _ -> do
              writeIORef playerStateRef ps' { pstTimeline = Nothing }
              Q.writeQueue evQ PlayerEventTimelineFinished
        else
          writeIORef playerStateRef ps' { pstTimeline = Just runtime'' }

advanceAutomationLanes ∷ Word64 → Queue PlayerEvent → PlayerState → IO PlayerState
advanceAutomationLanes now evQ ps0 = do
  let (ps1, energyEvents) = applyEnergyAutomation now ps0
      (ps2, moodEvents) = applyMoodAutomation now ps1
      ps3 = applyLiveTimelineSettings ps2
  mapM_ (Q.writeQueue evQ) (energyEvents <> moodEvents)
  pure ps3

applyEnergyAutomation ∷ Word64 → PlayerState → (PlayerState, [PlayerEvent])
applyEnergyAutomation now ps =
  case pstEnergyLane ps of
    Nothing -> (ps, [])
    Just lane ->
      let (mTarget, mLane, laneEvents) = stepEnergyLane now lane
          target' =
            case mTarget of
              Nothing -> pstEnergyTarget ps
              Just x -> x
          ps' =
            ps
              { pstEnergyTarget = target'
              , pstEnergyLane = mLane
              }
      in (ps', map toPlayerEnergyEvent laneEvents)
  where
    toPlayerEnergyEvent laneEvent =
      case laneEvent of
        EnergyLaneStarted startF endF fromE toE curve ->
          PlayerEventEnergyAutomationStarted startF endF fromE toE curve
        EnergyLaneCompleted endF finalE ->
          PlayerEventEnergyAutomationCompleted endF finalE

applyMoodAutomation ∷ Word64 → PlayerState → (PlayerState, [PlayerEvent])
applyMoodAutomation now ps =
  case pstMoodLane ps of
    Nothing -> (ps, [])
    Just lane ->
      let (mTarget, mLane, laneEvents) = stepMoodLane now lane
          target' =
            case mTarget of
              Nothing -> pstMoodTarget ps
              Just mood -> mood
          ps' =
            ps
              { pstMoodTarget = target'
              , pstMoodLane = mLane
              }
      in (ps', map toPlayerMoodEvent laneEvents)
  where
    toPlayerMoodEvent laneEvent =
      case laneEvent of
        MoodLaneStarted startF endF fromM toM ->
          PlayerEventMoodAutomationStarted startF endF fromM toM
        MoodLaneCompleted endF finalM ->
          PlayerEventMoodAutomationCompleted endF finalM

resolveNextBarBoundary ∷ AudioSystem → Word64 → PlayerState → (Word64, PlayerState)
resolveNextBarBoundary audioSys now ps =
  let fallback =
        nextBarFrame
          now
          (sampleRate (asHandle audioSys))
          (pstBeatsPerBar ps)
          (pstTempoBpm ps)
  in
    case pstTimeline ps of
      Nothing -> (fallback, ps)
      Just runtime ->
        let (mFrame, runtime') = timelineNextBarBoundaryFrame now runtime
            frame = maybe fallback id mFrame
        in (frame, ps { pstTimeline = Just runtime' })

mkEnergyLane
  ∷ AudioSystem
  → Word64
  → Float
  → Int
  → AutomationCurve
  → PlayerState
  → (EnergyLane, PlayerState)
mkEnergyLane audioSys now target bars curve ps =
  let fallback =
        scheduleEnergyLaneNextBar
          now
          (sampleRate (asHandle audioSys))
          (pstBeatsPerBar ps)
          (pstTempoBpm ps)
          (pstEnergyTarget ps)
          target
          bars
          curve
  in
    case pstTimeline ps of
      Nothing -> (fallback, ps)
      Just runtime ->
        let (mSpan, runtime') = timelineBoundarySpanFromNextBar now bars runtime
            lane =
              case mSpan of
                Just (startFrame, endFrame) ->
                  EnergyLane
                    { elStartFrame = startFrame
                    , elEndFrame = max startFrame endFrame
                    , elFromEnergy = clamp01 (pstEnergyTarget ps)
                    , elToEnergy = clamp01 target
                    , elCurve = curve
                    , elStarted = False
                    }
                Nothing -> fallback
        in (lane, ps { pstTimeline = Just runtime' })

mkMoodLane
  ∷ AudioSystem
  → Word64
  → Maybe String
  → Int
  → PlayerState
  → (MoodLane, PlayerState)
mkMoodLane audioSys now target bars ps =
  let fallback =
        scheduleMoodLaneNextBar
          now
          (sampleRate (asHandle audioSys))
          (pstBeatsPerBar ps)
          (pstTempoBpm ps)
          (pstMoodTarget ps)
          target
          bars
  in
    case pstTimeline ps of
      Nothing -> (fallback, ps)
      Just runtime ->
        let (mSpan, runtime') = timelineBoundarySpanFromNextBar now bars runtime
            lane =
              case mSpan of
                Just (startFrame, endFrame) ->
                  MoodLane
                    { mlStartFrame = startFrame
                    , mlEndFrame = max startFrame endFrame
                    , mlFromMood = pstMoodTarget ps
                    , mlToMood = target
                    , mlStarted = False
                    }
                Nothing -> fallback
        in (lane, ps { pstTimeline = Just runtime' })

scheduleTimelineBar
  ∷ AudioSystem
  → Queue PlayerEvent
  → TimelineRuntime
  → PlayerState
  → TimelineBar
  → IO PlayerState
scheduleTimelineBar audioSys evQ runtime ps bar = do
  let barStart = trStartFrame runtime + tbStartOffsetFrames bar
  (nextInst, _) <-
    foldM
      (scheduleNotePair audioSys evQ barStart)
      (pstNextNoteInst ps, 0 ∷ Int)
      (tbNotes bar)
  let ps' =
        ps
          { pstNextNoteInst = nextInst
          }
  pure ps'

scheduleNotePair
  ∷ AudioSystem
  → Queue PlayerEvent
  → Word64
  → (Word64, Int)
  → TimelineNote
  → IO (Word64, Int)
scheduleNotePair audioSys evQ barStart (nextInst, ix) note = do
  let (schedIid, schedKey, drumKey) = remapScheduledDrumNote (tnInstrumentId note) (tnKey note)
  case drumKey of
    Nothing -> pure ()
    Just key ->
      sendAudio audioSys (AudioLoadInstrument schedIid (gmDrumInstrument key))
  let noteInst = NoteInstanceId nextInst
      noteOnFrame = barStart + tnOnOffsetFrames note
      noteOffFrame = barStart + tnOffOffsetFrames note
      onAction =
        ScheduledNoteOn
          { saInstrumentId = schedIid
          , saAmp = tnAmp note
          , saPan = tnPan note
          , saNoteKey = schedKey
          , saNoteInstanceId = noteInst
          , saVelocity = tnVelocity note
          , saAdsrOverride = Nothing
          }
      offAction =
        ScheduledNoteOff
          { saInstrumentId = schedIid
          , saNoteInstanceId = noteInst
          }
  scheduleAudioActionAtFrame audioSys noteOnFrame onAction
  scheduleAudioActionAtFrame audioSys noteOffFrame offAction
  Q.writeQueue evQ (PlayerEventScheduled noteOnFrame onAction)
  Q.writeQueue evQ (PlayerEventScheduled noteOffFrame offAction)
  pure (nextInst + 1, ix + 1)

remapScheduledDrumNote ∷ InstrumentId → NoteKey → (InstrumentId, NoteKey, Maybe Int)
remapScheduledDrumNote iid key =
  case (iid, key) of
    (InstrumentId 9, NoteKey drumKey) ->
      (timelineDrumInstrumentId drumKey, NoteKey 60, Just drumKey)
    _ ->
      (iid, key, Nothing)

timelineDrumInstrumentId ∷ Int → InstrumentId
timelineDrumInstrumentId key =
  -- Reserve upper MIDI-range instrument slots for per-key drum patches.
  InstrumentId (128 + max 0 (min 127 key))

drainAudioEvents ∷ AudioSystem → Queue PlayerEvent → IO ()
drainAudioEvents audioSys evQ = do
  m <- tryReadAudioEvent audioSys
  case m of
    Nothing -> pure ()
    Just ev -> Q.writeQueue evQ (PlayerEventAudio ev) >> drainAudioEvents audioSys evQ

nextBarFrame ∷ Word64 → Word32 → Int → Float → Word64
nextBarFrame now sampleRateHz beatsPerBar bpm =
  let framesPerBar = fromIntegral (barFramesForTransport sampleRateHz beatsPerBar bpm) ∷ Integer
      nowI = fromIntegral now ∷ Integer
  in fromIntegral (((nowI `div` framesPerBar) + 1) * framesPerBar)
