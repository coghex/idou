{-# LANGUAGE Strict, UnicodeSyntax #-}

module Engine.Core.Queue
  ( Queue
  , newQueue
  , writeQueue
  , tryReadQueue
  , queueLength
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TV
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ

data Queue a = Queue
  { qItems ∷ !(TQueue a)
  , qSize  ∷ !(TVar Int)
  }

newQueue ∷ IO (Queue a)
newQueue = atomically $ do
  items <- TQ.newTQueue
  size <- TV.newTVar 0
  pure (Queue items size)

writeQueue ∷ Queue a → a → IO ()
writeQueue q a = atomically $ do
  TQ.writeTQueue (qItems q) a
  TV.modifyTVar' (qSize q) (+1)

tryReadQueue ∷ Queue a → IO (Maybe a)
tryReadQueue q = atomically $ do
  m <- TQ.tryReadTQueue (qItems q)
  case m of
    Nothing -> pure ()
    Just _  -> TV.modifyTVar' (qSize q) (\n -> max 0 (n - 1))
  pure m

queueLength ∷ Queue a → IO Int
queueLength q = atomically (TV.readTVar (qSize q))
