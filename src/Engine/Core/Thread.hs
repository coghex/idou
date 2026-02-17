{-# LANGUAGE Strict, UnicodeSyntax #-}

module Engine.Core.Thread
  ( ThreadControl(..)
  , ThreadState(..)
  ) where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)

data ThreadControl
  = ThreadRunning
  | ThreadPaused
  | ThreadStopped
  deriving (Eq, Show)

data ThreadState = ThreadState
  { tsRunning  ∷ IORef ThreadControl
  , tsThreadId ∷ ThreadId
  }
