{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Types
  ( AudioMsg(..)
  , AudioHandle(..)
  ) where

import Data.Word (Word32)
import Engine.Core.Queue (Queue)

data AudioMsg
  = AudioPlayBeep
      { amp     ∷ !Float   -- 0..1
      , pan     ∷ !Float   -- -1..1
      , freqHz  ∷ !Float
      , durSec  ∷ !Float
      }
  | AudioStopAll
  | AudioShutdown
  deriving (Eq, Show)

data AudioHandle = AudioHandle
  { audioQueue ∷ Queue AudioMsg
  , sampleRate ∷ !Word32
  , channels   ∷ !Word32
  }
