{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Thread
  ( PlayerMsg(..)
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
  , InstrumentId
  , NoteInstanceId(..)
  , NoteKey
  , ScheduledAudioAction(..)
  , sampleRate
  )
import Audio.Envelope (ADSR)
import Engine.Core.Queue (Queue)
import qualified Engine.Core.Queue as Q
import Engine.Core.Thread (ThreadControl(..), ThreadState(..))
import Player.Timeline
  ( SongSpec
  , TimelineBar(..)
  , TimelineNote(..)
  , TimelineRuntime(..)
  , compileTimelineBars
  , loadSongSpec
  , popReadyBars
  , prepareTimelineRuntime
  , timelineRuntimeDone
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
  | PlayerEventScheduled
      { peFrame ∷ !Word64
      , peAction ∷ !ScheduledAudioAction
      }
  deriving (Eq, Show)

data PlayerState = PlayerState
  { pstBeatsPerBar ∷ !Int
  , pstTempoBpm    ∷ !Float
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
  playerStateRef <- newIORef (PlayerState 4 120 Nothing Nothing 1)
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
  writeIORef (tsRunning (psThread ps)) ThreadStopped
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
          let bpm' = max 1 bpm
          modifyIORef' playerStateRef (\s -> s { pstTempoBpm = bpm' })
          sendAudio audioSys (AudioSetTransportBpm bpm')
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
                (\s -> s { pstSongSpec = Just spec, pstTimeline = Nothing })
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
                    prepareTimelineRuntime
                      (sampleRate (asHandle audioSys))
                      frame
                      spec
                  barCount = length (trBars runtime)
              if barCount <= 0
                then
                  Q.writeQueue
                    evQ
                    (PlayerEventTimelineLoadFailed "<runtime>" "loaded song has no timeline bars")
                else do
                  modifyIORef' playerStateRef (\s -> s { pstTimeline = Just runtime })
                  Q.writeQueue evQ (PlayerEventTimelineStarted frame barCount)
        PlayerStopSongTimeline -> do
          modifyIORef' playerStateRef (\s -> s { pstTimeline = Nothing })
          Q.writeQueue evQ PlayerEventTimelineStopped
        PlayerScheduleClipNextBar cid bus gain pan loop -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let frame =
                nextBarFrame
                  now
                  (sampleRate (asHandle audioSys))
                  (pstBeatsPerBar ps)
                  (pstTempoBpm ps)
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
          let frame =
                nextBarFrame
                  now
                  (sampleRate (asHandle audioSys))
                  (pstBeatsPerBar ps)
                  (pstTempoBpm ps)
              action = ScheduledSetBusGain bus gain
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
        PlayerScheduleNoteOnNextBar iid amp pan key noteInst vel adsrOverride -> do
          ps <- readIORef playerStateRef
          now <- readTransportFrame audioSys
          let frame =
                nextBarFrame
                  now
                  (sampleRate (asHandle audioSys))
                  (pstBeatsPerBar ps)
                  (pstTempoBpm ps)
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
          let frame =
                nextBarFrame
                  now
                  (sampleRate (asHandle audioSys))
                  (pstBeatsPerBar ps)
                  (pstTempoBpm ps)
              action =
                ScheduledNoteOff
                  { saInstrumentId = iid
                  , saNoteInstanceId = noteInst
                  }
          scheduleAudioActionAtFrame audioSys frame action
          Q.writeQueue evQ (PlayerEventScheduled frame action)
      processPlayerMsgs audioSys controlRef msgQ evQ playerStateRef

pumpTimeline
  ∷ AudioSystem
  → Queue PlayerEvent
  → IORef PlayerState
  → IO ()
pumpTimeline audioSys evQ playerStateRef = do
  ps <- readIORef playerStateRef
  case pstTimeline ps of
    Nothing -> pure ()
    Just runtime -> do
      now <- readTransportFrame audioSys
      let (readyBars, runtime') = popReadyBars now runtime
      ps' <- foldM (scheduleTimelineBar audioSys evQ runtime') ps readyBars
      if timelineRuntimeDone runtime'
        then do
          writeIORef playerStateRef ps' { pstTimeline = Nothing }
          Q.writeQueue evQ PlayerEventTimelineFinished
        else
          writeIORef playerStateRef ps' { pstTimeline = Just runtime' }

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
  let noteInst = NoteInstanceId nextInst
      noteOnFrame = barStart + tnOnOffsetFrames note
      noteOffFrame = barStart + tnOffOffsetFrames note
      onAction =
        ScheduledNoteOn
          { saInstrumentId = tnInstrumentId note
          , saAmp = tnAmp note
          , saPan = tnPan note
          , saNoteKey = tnKey note
          , saNoteInstanceId = noteInst
          , saVelocity = tnVelocity note
          , saAdsrOverride = Nothing
          }
      offAction =
        ScheduledNoteOff
          { saInstrumentId = tnInstrumentId note
          , saNoteInstanceId = noteInst
          }
  scheduleAudioActionAtFrame audioSys noteOnFrame onAction
  scheduleAudioActionAtFrame audioSys noteOffFrame offAction
  Q.writeQueue evQ (PlayerEventScheduled noteOnFrame onAction)
  Q.writeQueue evQ (PlayerEventScheduled noteOffFrame offAction)
  pure (nextInst + 1, ix + 1)

drainAudioEvents ∷ AudioSystem → Queue PlayerEvent → IO ()
drainAudioEvents audioSys evQ = do
  m <- tryReadAudioEvent audioSys
  case m of
    Nothing -> pure ()
    Just ev -> Q.writeQueue evQ (PlayerEventAudio ev) >> drainAudioEvents audioSys evQ

nextBarFrame ∷ Word64 → Word32 → Int → Float → Word64
nextBarFrame now sampleRateHz beatsPerBar bpm =
  let beats = max 1 beatsPerBar
      tempo = max 1 bpm
      framesPerBarD =
        (fromIntegral sampleRateHz ∷ Double)
          * 60
          * fromIntegral beats
          / realToFrac tempo
      framesPerBar = max 1 (floor framesPerBarD ∷ Integer)
      nowI = fromIntegral now ∷ Integer
  in fromIntegral (((nowI `div` framesPerBar) + 1) * framesPerBar)
