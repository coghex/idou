{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Types
  ( AudioMsg(..)
  , AudioHandle(..)
  , NoteId(..)
  ) where

import Data.Word (Word32)
import Engine.Core.Queue (Queue)
import Audio.Envelope (ADSR)

data NoteId
  = NoteMidi !Int      -- 0..127 typically
  | NoteHz   !Float
  deriving (Eq, Show)

data AudioMsg
  = AudioPlayBeep
      { amp     ∷ !Float   -- 0..1
      , pan     ∷ !Float   -- -1..1
      , freqHz  ∷ !Float
      , durSec  ∷ !Float   -- hold time before release
      , adsr    ∷ !ADSR
      }

  -- | Start a note (held until NoteOff).
  | AudioNoteOn
      { amp    ∷ !Float    -- 0..1 (or velocity mapped later)
      , pan    ∷ !Float    -- -1..1
      , noteId ∷ !NoteId
      , adsr   ∷ !ADSR
      }

  -- | Release a note. If multiple voices match, releases them all.
  | AudioNoteOff
      { noteId ∷ !NoteId
      }

  | AudioStopAll
  | AudioShutdown
  deriving (Eq, Show)

data AudioHandle = AudioHandle
  { audioQueue ∷ Queue AudioMsg
  , sampleRate ∷ !Word32
  , channels   ∷ !Word32
  }
