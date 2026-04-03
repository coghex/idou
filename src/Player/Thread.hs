{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Thread
  ( AutomationCurve(..)
  , PlayerEvent(..)
  , PlayerRuntimeStatus(..)
  , PlayerSystem(..)
  , nextBarFrame
  , startPlayerThread
  , stopPlayerThread
  , tryReadPlayerEvent
  , pausePlayer
  , resumePlayer
  , setPlayerMeter
  , setPlayerTempoBpm
  , setPlayerGenreTarget
  , clearPlayerGenreTarget
  , setPlayerMoodTarget
  , clearPlayerMoodTarget
  , setPlayerEnergyTarget
  , automatePlayerEnergyNextBar
  , automatePlayerMoodNextBar
  , cancelPlayerEnergyAutomation
  , cancelPlayerMoodAutomation
  , loadPlayerSongTimeline
  , loadPlayerSongTimelineChecked
  , startPlayerSongTimeline
  , startPlayerSongTimelineChecked
  , stopPlayerSongTimeline
  , loadPlayerClipChecked
  , schedulePlayerClipNextBar
  , schedulePlayerBusGainNextBar
  , schedulePlayerNoteOnNextBar
  , schedulePlayerNoteOffNextBar
  , loadPlayerClip
  , playPlayerClipNow
  , stopPlayerClipNow
  , setPlayerBusGainNow
  , stopPlayerBusNow
  , playerNoteOnNow
  , playerNoteOffNow
  , panicPlayer
  , requestPlayerStatus
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (foldM)
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)

import qualified Data.Sequence as Seq

import Audio.Thread
  ( AudioSystem(..)
  , loadClipFromFile
  , readTransportFrame
  , scheduleAudioActionAtFrame
  , sendAudio
  , tryReadAudioEvent
  )
import Audio.Types
  ( AudioBus(..)
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
import Player.Instrument (loadResolvedInstrumentEither, lookupInstrumentSpec, usesBuiltInDrumPatch)
import Player.Timeline
  ( InstrumentPatternSpec(..)
  , SongSpec
  , TimelineBar(..)
  , TimelineLookaheadTelemetry(..)
  , TimelineNote(..)
  , TimelineRetargetTelemetry
  , TimelineTransitionTelemetry
  , TimelineRuntime(..)
  , compileTimelineBars
  , loadSongSpec
  , popLookaheadTelemetry
  , popRetargetTelemetry
  , popTransitionTelemetry
  , popReadyBars
  , prepareTimelineRuntime
  , setTimelineGenre
  , setTimelineTargets
  , sgGenre
  , sgInstruments
  , stampLookaheadPerfMetrics
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
  | PlayerRequestStatus
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
  | PlayerLoadSongTimelineChecked
      { pmSongPath ∷ !FilePath
      , pmSongReply ∷ !(MVar (Either String ()))
      }
  | PlayerLoadClip
      { pmLoadClipId   ∷ !ClipId
      , pmLoadClipPath ∷ !FilePath
      }
  | PlayerLoadClipChecked
      { pmLoadClipId   ∷ !ClipId
      , pmLoadClipPath ∷ !FilePath
      , pmLoadClipReply ∷ !(MVar (Either String ()))
      }
  | PlayerStartSongTimeline
  | PlayerStartSongTimelineChecked
      { pmStartSongReply ∷ !(MVar (Either String ()))
      }
  | PlayerStopSongTimeline
  | PlayerPlayClipNow
      { pmPlayClipId   ∷ !ClipId
      , pmPlayClipBus  ∷ !AudioBus
      , pmPlayClipGain ∷ !Float
      , pmPlayClipPan  ∷ !Float
      , pmPlayClipLoop ∷ !Bool
      }
  | PlayerStopClipNow
      { pmStopClipId ∷ !ClipId
      }
  | PlayerSetBusGainNow
      { pmSetGainNowBus   ∷ !AudioBus
      , pmSetGainNowValue ∷ !Float
      }
  | PlayerStopBusNow
      { pmStopNowBus ∷ !AudioBus
      }
  | PlayerNoteOnNow
      { pmNowBus            ∷ !AudioBus
      , pmNowInstrumentId   ∷ !InstrumentId
      , pmNowAmp            ∷ !Float
      , pmNowPan            ∷ !Float
      , pmNowKey            ∷ !NoteKey
      , pmNowInstanceId     ∷ !NoteInstanceId
      , pmNowVelocity       ∷ !Float
      , pmNowAdsrOverride   ∷ !(Maybe ADSR)
      }
  | PlayerNoteOffNow
      { pmNowOffInstrumentId ∷ !InstrumentId
      , pmNowOffInstanceId   ∷ !NoteInstanceId
      }
  | PlayerPanic
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
      { pmNoteOnBus ∷ !AudioBus
      , pmNoteOnInstrumentId ∷ !InstrumentId
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

data PlayerRuntimeStatus = PlayerRuntimeStatus
  { prsTransportFrame    ∷ !Word64
  , prsSongLoaded        ∷ !Bool
  , prsTimelinePlaying   ∷ !Bool
  , prsCurrentSection    ∷ !(Maybe String)
  , prsTimelineStartFrame ∷ !(Maybe Word64)
  , prsTimelineEndFrame  ∷ !(Maybe Word64)
  , prsBufferedFutureBars ∷ !Int
  , prsTempoBpm          ∷ !Float
  , prsBeatsPerBar       ∷ !Int
  , prsMoodTarget        ∷ !(Maybe String)
  , prsEnergyTarget      ∷ !Float
  , prsGenre             ∷ !(Maybe String)
  }
  deriving (Eq, Show)

data PlayerEvent
  = PlayerEventAudio !AudioEvent
  | PlayerEventStatus !PlayerRuntimeStatus
  | PlayerEventTimelineLoaded !FilePath
  | PlayerEventTimelineLoadFailed !FilePath !String
  | PlayerEventClipLoaded !ClipId !FilePath
  | PlayerEventClipLoadFailed !ClipId !FilePath !String
  | PlayerEventTimelineStarted
      { peTimelineStartFrame ∷ !Word64
      , peTimelineBarCount   ∷ !Int
      }
  | PlayerEventTimelineStopped
  | PlayerEventTimelineFinished
  | PlayerEventTimelineRetargeted !TimelineRetargetTelemetry
  | PlayerEventTimelineLookahead !TimelineLookaheadTelemetry
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

pausePlayer ∷ PlayerSystem → IO ()
pausePlayer ps = sendPlayer ps PlayerPause

resumePlayer ∷ PlayerSystem → IO ()
resumePlayer ps = sendPlayer ps PlayerResume

setPlayerMeter ∷ PlayerSystem → Int → IO ()
setPlayerMeter ps beats = sendPlayer ps (PlayerSetMeter beats)

setPlayerTempoBpm ∷ PlayerSystem → Float → IO ()
setPlayerTempoBpm ps bpm = sendPlayer ps (PlayerSetTempoBpm bpm)

setPlayerGenreTarget ∷ PlayerSystem → String → IO ()
setPlayerGenreTarget ps genre = sendPlayer ps (PlayerSetGenreTarget genre)

clearPlayerGenreTarget ∷ PlayerSystem → IO ()
clearPlayerGenreTarget ps = sendPlayer ps PlayerClearGenreTarget

setPlayerMoodTarget ∷ PlayerSystem → String → IO ()
setPlayerMoodTarget ps mood = sendPlayer ps (PlayerSetMoodTarget mood)

clearPlayerMoodTarget ∷ PlayerSystem → IO ()
clearPlayerMoodTarget ps = sendPlayer ps PlayerClearMoodTarget

setPlayerEnergyTarget ∷ PlayerSystem → Float → IO ()
setPlayerEnergyTarget ps energy = sendPlayer ps (PlayerSetEnergyTarget energy)

automatePlayerEnergyNextBar ∷ PlayerSystem → Float → Int → AutomationCurve → IO ()
automatePlayerEnergyNextBar ps target bars curve =
  sendPlayer ps (PlayerAutomateEnergyNextBar target bars curve)

automatePlayerMoodNextBar ∷ PlayerSystem → String → Int → IO ()
automatePlayerMoodNextBar ps mood bars =
  sendPlayer ps (PlayerAutomateMoodNextBar mood bars)

cancelPlayerEnergyAutomation ∷ PlayerSystem → IO ()
cancelPlayerEnergyAutomation ps = sendPlayer ps PlayerCancelEnergyAutomation

cancelPlayerMoodAutomation ∷ PlayerSystem → IO ()
cancelPlayerMoodAutomation ps = sendPlayer ps PlayerCancelMoodAutomation

loadPlayerSongTimeline ∷ PlayerSystem → FilePath → IO ()
loadPlayerSongTimeline ps path = sendPlayer ps (PlayerLoadSongTimeline path)

loadPlayerSongTimelineChecked ∷ PlayerSystem → FilePath → IO (Either String ())
loadPlayerSongTimelineChecked ps path = do
  reply <- newEmptyMVar
  sendPlayer ps (PlayerLoadSongTimelineChecked path reply)
  takeMVar reply

loadPlayerClip ∷ PlayerSystem → ClipId → FilePath → IO ()
loadPlayerClip ps clipId path = sendPlayer ps (PlayerLoadClip clipId path)

loadPlayerClipChecked ∷ PlayerSystem → ClipId → FilePath → IO (Either String ())
loadPlayerClipChecked ps clipId path = do
  reply <- newEmptyMVar
  sendPlayer ps (PlayerLoadClipChecked clipId path reply)
  takeMVar reply

startPlayerSongTimeline ∷ PlayerSystem → IO ()
startPlayerSongTimeline ps = sendPlayer ps PlayerStartSongTimeline

startPlayerSongTimelineChecked ∷ PlayerSystem → IO (Either String ())
startPlayerSongTimelineChecked ps = do
  reply <- newEmptyMVar
  sendPlayer ps (PlayerStartSongTimelineChecked reply)
  takeMVar reply

stopPlayerSongTimeline ∷ PlayerSystem → IO ()
stopPlayerSongTimeline ps = sendPlayer ps PlayerStopSongTimeline

playPlayerClipNow ∷ PlayerSystem → ClipId → AudioBus → Float → Float → Bool → IO ()
playPlayerClipNow ps clipId bus gain pan loop =
  sendPlayer ps (PlayerPlayClipNow clipId bus gain pan loop)

stopPlayerClipNow ∷ PlayerSystem → ClipId → IO ()
stopPlayerClipNow ps clipId = sendPlayer ps (PlayerStopClipNow clipId)

setPlayerBusGainNow ∷ PlayerSystem → AudioBus → Float → IO ()
setPlayerBusGainNow ps bus gain = sendPlayer ps (PlayerSetBusGainNow bus gain)

stopPlayerBusNow ∷ PlayerSystem → AudioBus → IO ()
stopPlayerBusNow ps bus = sendPlayer ps (PlayerStopBusNow bus)

playerNoteOnNow
  ∷ PlayerSystem
  → AudioBus
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
playerNoteOnNow ps bus iid amp pan key noteInst velocity adsrOverride =
  sendPlayer ps (PlayerNoteOnNow bus iid amp pan key noteInst velocity adsrOverride)

playerNoteOffNow ∷ PlayerSystem → InstrumentId → NoteInstanceId → IO ()
playerNoteOffNow ps iid noteInst =
  sendPlayer ps (PlayerNoteOffNow iid noteInst)

panicPlayer ∷ PlayerSystem → IO ()
panicPlayer ps = sendPlayer ps PlayerPanic

schedulePlayerClipNextBar ∷ PlayerSystem → ClipId → AudioBus → Float → Float → Bool → IO ()
schedulePlayerClipNextBar ps clipId bus gain pan loop =
  sendPlayer ps (PlayerScheduleClipNextBar clipId bus gain pan loop)

schedulePlayerBusGainNextBar ∷ PlayerSystem → AudioBus → Float → IO ()
schedulePlayerBusGainNextBar ps bus gain =
  sendPlayer ps (PlayerScheduleBusGainNextBar bus gain)

schedulePlayerNoteOnNextBar
  ∷ PlayerSystem
  → AudioBus
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
schedulePlayerNoteOnNextBar ps bus iid amp pan key noteInst velocity adsrOverride =
  sendPlayer ps (PlayerScheduleNoteOnNextBar bus iid amp pan key noteInst velocity adsrOverride)

schedulePlayerNoteOffNextBar ∷ PlayerSystem → InstrumentId → NoteInstanceId → IO ()
schedulePlayerNoteOffNextBar ps iid noteInst =
  sendPlayer ps (PlayerScheduleNoteOffNextBar iid noteInst)

requestPlayerStatus ∷ PlayerSystem → IO ()
requestPlayerStatus ps = sendPlayer ps PlayerRequestStatus

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
          flushPendingTimelineTelemetry evQ playerStateRef
          drainAudioEvents audioSys evQ
        ThreadPaused -> do
          processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef
          flushPendingTimelineTelemetry evQ playerStateRef
          threadDelay 10000
          loop
        ThreadRunning -> do
          processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef
          flushPendingTimelineTelemetry evQ playerStateRef
          pumpTimeline audioSys evQ playerStateRef
          flushPendingTimelineTelemetry evQ playerStateRef
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
        PlayerRequestStatus -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          Q.writeQueue evQ (PlayerEventStatus (snapshotPlayerRuntimeStatus now ps))
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
              let ps' = applyLiveTimelineSettings (ps { pstGenreTarget = genre' })
              preloadResult <- preloadActiveTimelineInstruments audioSys ps'
              case preloadResult of
                Left err ->
                  Q.writeQueue evQ (PlayerEventGenreChangeRejected genreRaw err)
                Right () -> do
                  writeIORef playerStateRef ps'
                  Q.writeQueue evQ (PlayerEventGenreChanged currentGenre)
        PlayerClearGenreTarget -> do
          ps <- readIORef playerStateRef
          case validateGenreTarget ps Nothing of
            Left err ->
              Q.writeQueue evQ (PlayerEventGenreChangeRejected "<default>" err)
            Right currentGenre -> do
              let ps' = applyLiveTimelineSettings (ps { pstGenreTarget = Nothing })
              preloadResult <- preloadActiveTimelineInstruments audioSys ps'
              case preloadResult of
                Left err ->
                  Q.writeQueue evQ (PlayerEventGenreChangeRejected "<default>" err)
                Right () -> do
                  writeIORef playerStateRef ps'
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
          (lane, ps') <- mkEnergyLane audioSys now target bars curve ps
          writeIORef playerStateRef ps' { pstEnergyLane = Just lane }
        PlayerAutomateMoodNextBar moodRaw bars -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let target = normalizeMoodTarget moodRaw
          (lane, ps') <- mkMoodLane audioSys now target bars ps
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
        PlayerLoadSongTimeline path ->
          handleLoadSongTimeline path Nothing
        PlayerLoadSongTimelineChecked path reply ->
          handleLoadSongTimeline path (Just reply)
        PlayerLoadClip clipId path -> do
          handleLoadClip clipId path Nothing
        PlayerLoadClipChecked clipId path reply ->
          handleLoadClip clipId path (Just reply)
        PlayerStartSongTimeline ->
          handleStartSongTimeline Nothing
        PlayerStartSongTimelineChecked reply ->
          handleStartSongTimeline (Just reply)
        PlayerStopSongTimeline -> do
          modifyIORef' playerStateRef (\s -> s { pstTimeline = Nothing })
          sendAudio audioSys AudioStopAll
          Q.writeQueue evQ PlayerEventTimelineStopped
        PlayerPlayClipNow clipId bus gain pan loop ->
          sendAudio
            audioSys
            (AudioPlayClip
              { clipId = clipId
              , clipBus = bus
              , clipGain = gain
              , clipPan = pan
              , clipLoop = loop
              })
        PlayerStopClipNow clipId ->
          sendAudio audioSys (AudioStopClip clipId)
        PlayerSetBusGainNow bus gain ->
          sendAudio audioSys (AudioSetBusGain bus gain)
        PlayerStopBusNow bus ->
          sendAudio audioSys (AudioStopBus bus)
        PlayerNoteOnNow bus iid amp pan key noteInst velocity adsrOverride ->
          sendAudio
            audioSys
            (AudioNoteOn
              { instrumentId = iid
              , noteBus = bus
              , amp = amp
              , pan = pan
              , noteKey = key
              , noteInstanceId = noteInst
              , velocity = velocity
              , adsrOverride = adsrOverride
              })
        PlayerNoteOffNow iid noteInst ->
          sendAudio
            audioSys
            (AudioNoteOff
              { instrumentId = iid
              , noteInstanceId = noteInst
              })
        PlayerPanic ->
          sendAudio audioSys AudioStopAll
        PlayerScheduleClipNextBar cid bus gain pan loop -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          (frame, ps') <- resolveNextBarBoundary audioSys now ps
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
          (frame, ps') <- resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action = ScheduledSetBusGain bus gain
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
        PlayerScheduleNoteOnNextBar bus iid amp pan key noteInst vel adsrOverride -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          (frame, ps') <- resolveNextBarBoundary audioSys now ps
          writeIORef playerStateRef ps'
          let
              action =
                ScheduledNoteOn
                  { saInstrumentId = iid
                  , saNoteBus = bus
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
          (frame, ps') <- resolveNextBarBoundary audioSys now ps
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
  where
    finishReply :: Maybe (MVar (Either String ())) -> Either String () -> IO ()
    finishReply mReply result =
      case mReply of
        Nothing -> pure ()
        Just reply -> putMVar reply result

    handleLoadSongTimeline :: FilePath -> Maybe (MVar (Either String ())) -> IO ()
    handleLoadSongTimeline path mReply = do
      loaded <- loadSongSpec path
      case loaded of
        Left err -> do
          Q.writeQueue evQ (PlayerEventTimelineLoadFailed path err)
          finishReply mReply (Left err)
        Right spec -> do
          preloadResult <- preloadInstrumentSpecs audioSys (sgInstruments spec)
          case preloadResult of
            Left err -> do
              Q.writeQueue evQ (PlayerEventTimelineLoadFailed path err)
              finishReply mReply (Left err)
            Right () -> do
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
              finishReply mReply (Right ())

    handleStartSongTimeline :: Maybe (MVar (Either String ())) -> IO ()
    handleStartSongTimeline mReply = do
      ps <- readIORef playerStateRef
      case pstSongSpec ps of
        Nothing -> do
          let err = "no song loaded"
          Q.writeQueue evQ (PlayerEventTimelineLoadFailed "<runtime>" err)
          finishReply mReply (Left err)
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
          preloadResult <- preloadInstrumentSpecs audioSys (trInstruments runtime)
          case preloadResult of
            Left err -> do
              Q.writeQueue evQ (PlayerEventTimelineLoadFailed "<runtime>" err)
              finishReply mReply (Left err)
            Right () -> do
              modifyIORef' playerStateRef (\s -> s { pstTimeline = Just runtime })
              Q.writeQueue evQ (PlayerEventTimelineStarted frame estimatedBars)
              finishReply mReply (Right ())

    handleLoadClip :: ClipId -> FilePath -> Maybe (MVar (Either String ())) -> IO ()
    handleLoadClip clipId path mReply = do
      loaded <- try (loadClipFromFile audioSys clipId path) ∷ IO (Either SomeException ())
      case loaded of
        Left ex -> do
          let err = displayException ex
          Q.writeQueue evQ (PlayerEventClipLoadFailed clipId path err)
          finishReply mReply (Left err)
        Right () -> do
          Q.writeQueue evQ (PlayerEventClipLoaded clipId path)
          finishReply mReply (Right ())

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

preloadActiveTimelineInstruments ∷ AudioSystem → PlayerState → IO (Either String ())
preloadActiveTimelineInstruments audioSys ps =
  case pstTimeline ps of
    Nothing -> pure (Right ())
    Just timeline -> preloadInstrumentSpecs audioSys (trInstruments timeline)

preloadInstrumentSpecs ∷ AudioSystem → [InstrumentPatternSpec] → IO (Either String ())
preloadInstrumentSpecs audioSys = go
  where
    go [] = pure (Right ())
    go (spec : rest) = do
      result <- preloadInstrument spec
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

    preloadInstrument spec =
      if usesBuiltInDrumPatch spec
        then pure (Right ())
        else do
          instResult <- loadResolvedInstrumentEither spec
          case instResult of
            Left err ->
              pure (Left err)
            Right inst -> do
              sendAudio audioSys (AudioLoadInstrument (ipInstrumentId spec) inst)
              pure (Right ())

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

snapshotPlayerRuntimeStatus ∷ Word64 → PlayerState → PlayerRuntimeStatus
snapshotPlayerRuntimeStatus now ps =
  let timeline = pstTimeline ps
      (playing, currentSection, startFrame, endFrame, bufferedBars) =
        case timeline of
          Nothing -> (False, Nothing, Nothing, Nothing, 0)
          Just rt ->
            ( True
            , nonEmptyTextMaybe (trCurrentSectionName rt)
            , Just (trStartFrame rt)
            , timelineRuntimeEndFrame rt
            , futureBufferedBars rt
            )
  in PlayerRuntimeStatus
       { prsTransportFrame = now
       , prsSongLoaded = maybe False (const True) (pstSongSpec ps)
       , prsTimelinePlaying = playing
       , prsCurrentSection = currentSection
       , prsTimelineStartFrame = startFrame
       , prsTimelineEndFrame = endFrame
       , prsBufferedFutureBars = bufferedBars
       , prsTempoBpm = pstTempoBpm ps
       , prsBeatsPerBar = pstBeatsPerBar ps
       , prsMoodTarget = pstMoodTarget ps
       , prsEnergyTarget = pstEnergyTarget ps
       , prsGenre = activeGenreTarget ps
       }

futureBufferedBars ∷ TimelineRuntime → Int
futureBufferedBars rt = max 0 (Seq.length (Seq.drop seqIx (trBars rt)))
  where
    seqIx = max 0 (trNextBarIx rt - trBarOffset rt)

nonEmptyTextMaybe ∷ String → Maybe String
nonEmptyTextMaybe raw =
  let txt = trim raw
  in if null txt then Nothing else Just txt

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
      (readyBars, runtime1) <- timedPopReadyBars now runtime
      let (retargetTelemetry, runtime2) = popRetargetTelemetry runtime1
          (lookaheadTelemetry, runtime3) = popLookaheadTelemetry runtime2
          (transitionTelemetry, runtime4) = popTransitionTelemetry runtime3
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineRetargeted) retargetTelemetry
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineLookahead) lookaheadTelemetry
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineTransition) transitionTelemetry
      ps' <- foldM (scheduleTimelineBar audioSys evQ runtime4) ps readyBars
      if timelineRuntimeDone runtime4
        then
          case timelineRuntimeEndFrame runtime4 of
            Just endFrame
              | now < endFrame ->
                  writeIORef playerStateRef ps' { pstTimeline = Just runtime4 }
            _ -> do
              writeIORef playerStateRef ps' { pstTimeline = Nothing }
              Q.writeQueue evQ PlayerEventTimelineFinished
        else
          writeIORef playerStateRef ps' { pstTimeline = Just runtime4 }

flushPendingTimelineTelemetry ∷ Queue PlayerEvent → IORef PlayerState → IO ()
flushPendingTimelineTelemetry evQ playerStateRef = do
  ps <- readIORef playerStateRef
  case pstTimeline ps of
    Nothing -> pure ()
    Just runtime -> do
      let (retargetTelemetry, runtime1) = popRetargetTelemetry runtime
          (lookaheadTelemetry, runtime2) = popLookaheadTelemetry runtime1
          (transitionTelemetry, runtime3) = popTransitionTelemetry runtime2
      writeIORef playerStateRef ps { pstTimeline = Just runtime3 }
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineRetargeted) retargetTelemetry
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineLookahead) lookaheadTelemetry
      mapM_ (Q.writeQueue evQ . PlayerEventTimelineTransition) transitionTelemetry

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

resolveNextBarBoundary ∷ AudioSystem → Word64 → PlayerState → IO (Word64, PlayerState)
resolveNextBarBoundary audioSys now ps =
  let fallback =
        nextBarFrame
          now
          (sampleRate (asHandle audioSys))
          (pstBeatsPerBar ps)
          (pstTempoBpm ps)
  in
    case pstTimeline ps of
      Nothing -> pure (fallback, ps)
      Just runtime -> do
        (mFrame, runtime') <- timedNextBarBoundary now runtime
        let frame = maybe fallback id mFrame
        pure (frame, ps { pstTimeline = Just runtime' })

mkEnergyLane
  ∷ AudioSystem
  → Word64
  → Float
  → Int
  → AutomationCurve
  → PlayerState
  → IO (EnergyLane, PlayerState)
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
      Nothing -> pure (fallback, ps)
      Just runtime -> do
        (mSpan, runtime') <- timedBoundarySpan now bars runtime
        let lane =
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
        pure (lane, ps { pstTimeline = Just runtime' })

mkMoodLane
  ∷ AudioSystem
  → Word64
  → Maybe String
  → Int
  → PlayerState
  → IO (MoodLane, PlayerState)
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
      Nothing -> pure (fallback, ps)
      Just runtime -> do
        (mSpan, runtime') <- timedBoundarySpan now bars runtime
        let lane =
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
        pure (lane, ps { pstTimeline = Just runtime' })

timedPopReadyBars ∷ Word64 → TimelineRuntime → IO ([TimelineBar], TimelineRuntime)
timedPopReadyBars now runtime = do
  startNs <- getMonotonicTimeNSec
  let (readyBars, runtime') = popReadyBars now runtime
  _ <- evaluate (length readyBars)
  _ <- evaluate (trGeneratedBarCount runtime')
  _ <- evaluate (length (trPendingLookahead runtime'))
  endNs <- getMonotonicTimeNSec
  pure
    ( readyBars
    , annotateNewLookaheadPerfMetrics
        (nanosecondsToMicroseconds (endNs - startNs))
        runtime
        runtime'
    )

timedNextBarBoundary ∷ Word64 → TimelineRuntime → IO (Maybe Word64, TimelineRuntime)
timedNextBarBoundary now runtime = do
  startNs <- getMonotonicTimeNSec
  let (mFrame, runtime') = timelineNextBarBoundaryFrame now runtime
  _ <- evaluate mFrame
  _ <- evaluate (trGeneratedBarCount runtime')
  _ <- evaluate (length (trPendingLookahead runtime'))
  endNs <- getMonotonicTimeNSec
  pure
    ( mFrame
    , annotateNewLookaheadPerfMetrics
        (nanosecondsToMicroseconds (endNs - startNs))
        runtime
        runtime'
    )

timedBoundarySpan ∷ Word64 → Int → TimelineRuntime → IO (Maybe (Word64, Word64), TimelineRuntime)
timedBoundarySpan now bars runtime = do
  startNs <- getMonotonicTimeNSec
  let (mSpan, runtime') = timelineBoundarySpanFromNextBar now bars runtime
  _ <- evaluate mSpan
  _ <- evaluate (trGeneratedBarCount runtime')
  _ <- evaluate (length (trPendingLookahead runtime'))
  endNs <- getMonotonicTimeNSec
  pure
    ( mSpan
    , annotateNewLookaheadPerfMetrics
        (nanosecondsToMicroseconds (endNs - startNs))
        runtime
        runtime'
    )

annotateNewLookaheadPerfMetrics ∷ Word64 → TimelineRuntime → TimelineRuntime → TimelineRuntime
annotateNewLookaheadPerfMetrics durationUs before after =
  let beforeCount = length (trPendingLookahead before)
      afterPending = trPendingLookahead after
      newCount = max 0 (length afterPending - beforeCount)
      (newEvents, priorEvents) = splitAt newCount afterPending
  in
    if newCount <= 0
      then after
      else
        after
          { trPendingLookahead =
              map (stampLookaheadPerfMetrics durationUs) newEvents <> priorEvents
          }

nanosecondsToMicroseconds ∷ Word64 → Word64
nanosecondsToMicroseconds ns = ns `div` 1000

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
      (scheduleNotePair audioSys evQ runtime barStart)
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
  → TimelineRuntime
  → Word64
  → (Word64, Int)
  → TimelineNote
  → IO (Word64, Int)
scheduleNotePair audioSys evQ runtime barStart (nextInst, ix) note = do
  let spec = lookupInstrumentSpec (tnInstrumentId note) (trInstruments runtime)
      (schedIid, schedKey, drumKey) = remapScheduledDrumNote spec (tnInstrumentId note) (tnKey note)
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
          , saNoteBus = AudioBusMusic
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

remapScheduledDrumNote ∷ Maybe InstrumentPatternSpec → InstrumentId → NoteKey → (InstrumentId, NoteKey, Maybe Int)
remapScheduledDrumNote spec iid key =
  case (shouldUseBuiltInDrums spec iid, key) of
    (True, NoteKey drumKey) ->
      (timelineDrumInstrumentId drumKey, NoteKey 60, Just drumKey)
    _ ->
      (iid, key, Nothing)

shouldUseBuiltInDrums ∷ Maybe InstrumentPatternSpec → InstrumentId → Bool
shouldUseBuiltInDrums spec iid =
  case spec of
    Just instrumentSpec -> usesBuiltInDrumPatch instrumentSpec
    Nothing ->
      case iid of
        InstrumentId 9 -> True
        _ -> False

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
