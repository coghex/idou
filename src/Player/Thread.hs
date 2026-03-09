{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Thread
  ( PlayerMsg(..)
  , PlayerEvent(..)
  , PlayerSystem(..)
  , startPlayerThread
  , stopPlayerThread
  , sendPlayer
  , tryReadPlayerEvent
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Audio.Thread (AudioSystem, tryReadAudioEvent)
import Audio.Types (AudioEvent)
import Engine.Core.Queue (Queue)
import qualified Engine.Core.Queue as Q
import Engine.Core.Thread (ThreadControl(..), ThreadState(..))

data PlayerMsg
  = PlayerShutdown
  | PlayerPause
  | PlayerResume
  deriving (Eq, Show)

data PlayerEvent
  = PlayerEventAudio !AudioEvent
  deriving (Eq, Show)

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
  doneVar <- newEmptyMVar
  tid <- forkIO $
    runPlayerLoop audioSys msgQ evQ controlRef `finally` putMVar doneVar ()
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
  → IO ()
runPlayerLoop audioSys msgQ evQ controlRef = loop
  where
    loop = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> do
          processPlayerMsgs controlRef msgQ
          drainAudioEvents audioSys evQ
        ThreadPaused -> do
          processPlayerMsgs controlRef msgQ
          threadDelay 10000
          loop
        ThreadRunning -> do
          processPlayerMsgs controlRef msgQ
          drainAudioEvents audioSys evQ
          threadDelay 1000
          loop

processPlayerMsgs ∷ IORef ThreadControl → Queue PlayerMsg → IO ()
processPlayerMsgs controlRef msgQ = do
  m <- Q.tryReadQueue msgQ
  case m of
    Nothing -> pure ()
    Just msg -> do
      case msg of
        PlayerShutdown -> writeIORef controlRef ThreadStopped
        PlayerPause -> writeIORef controlRef ThreadPaused
        PlayerResume -> writeIORef controlRef ThreadRunning
      processPlayerMsgs controlRef msgQ

drainAudioEvents ∷ AudioSystem → Queue PlayerEvent → IO ()
drainAudioEvents audioSys evQ = do
  m <- tryReadAudioEvent audioSys
  case m of
    Nothing -> pure ()
    Just ev -> Q.writeQueue evQ (PlayerEventAudio ev) >> drainAudioEvents audioSys evQ
