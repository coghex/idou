{-# LANGUAGE Strict, UnicodeSyntax #-}

module Engine.Core.Thread
  ( ThreadControl(..)
  , ThreadState(..)
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)

data ThreadControl
  = ThreadRunning
  | ThreadPaused
  | ThreadStopped
  deriving (Eq, Show)

data ThreadState = ThreadState
  { tsRunning  ∷ IORef ThreadControl
  , tsThreadId ∷ ThreadId
  , tsDone     ∷ MVar ()
  }
