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
  , NoteInstanceId
  , NoteKey
  , ScheduledAudioAction(..)
  , sampleRate
  )
import Audio.Envelope (ADSR)
import Engine.Core.Queue (Queue)
import qualified Engine.Core.Queue as Q
import Engine.Core.Thread (ThreadControl(..), ThreadState(..))

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
  | PlayerEventScheduled
      { peFrame ∷ !Word64
      , peAction ∷ !ScheduledAudioAction
      }
  deriving (Eq, Show)

data PlayerState = PlayerState
  { pstBeatsPerBar ∷ !Int
  , pstTempoBpm    ∷ !Float
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
  playerStateRef <- newIORef (PlayerState 4 120)
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
