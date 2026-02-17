{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Envelope
  ( ADSR(..)
  , EnvStage(..)
  , EnvState(..)
  , envInit
  , envRelease
  , envStep
  ) where

data ADSR = ADSR
  { aAttackSec  ∷ !Float  -- >= 0
  , aDecaySec   ∷ !Float  -- >= 0
  , aSustain    ∷ !Float  -- 0..1
  , aReleaseSec ∷ !Float  -- >= 0
  } deriving (Eq, Show)

data EnvStage = EnvAttack | EnvDecay | EnvSustain | EnvRelease | EnvDone
  deriving (Eq, Show)

data EnvState = EnvState
  { eStage ∷ !EnvStage
  , eLevel ∷ !Float
  } deriving (Eq, Show)

envInit ∷ EnvState
envInit = EnvState EnvAttack 0

envRelease ∷ EnvState → EnvState
envRelease st =
  case eStage st of
    EnvDone    -> st
    EnvRelease -> st
    _          -> st { eStage = EnvRelease }

-- | Step one sample; return (newState, currentLevel).
envStep ∷ Float → ADSR → EnvState → (EnvState, Float)
envStep sr adsr st0 =
  case eStage st0 of
    EnvAttack ->
      let inc = if aAttackSec adsr <= 0 then 1 else 1 / (aAttackSec adsr * sr)
          lvl' = min 1 (eLevel st0 + inc)
      in if lvl' >= 1
           then (EnvState EnvDecay 1, 1)
           else (st0 { eLevel = lvl' }, lvl')

    EnvDecay ->
      let target = clamp01 (aSustain adsr)
          dec = if aDecaySec adsr <= 0 then 1 else 1 / (aDecaySec adsr * sr)
          lvl' = max target (eLevel st0 - dec)
      in if lvl' <= target
           then (EnvState EnvSustain target, target)
           else (st0 { eLevel = lvl' }, lvl')

    EnvSustain ->
      (st0, eLevel st0)

    EnvRelease ->
      let dec = if aReleaseSec adsr <= 0 then 1 else 1 / (aReleaseSec adsr * sr)
          lvl' = max 0 (eLevel st0 - dec)
      in if lvl' <= 0
           then (EnvState EnvDone 0, 0)
           else (st0 { eLevel = lvl' }, lvl')

    EnvDone ->
      (st0, 0)

clamp01 ∷ Float → Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
