{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Automation
  ( AutomationCurve(..)
  , EnergyLane(..)
  , MoodLane(..)
  , EnergyLaneEvent(..)
  , MoodLaneEvent(..)
  , barFramesForTransport
  , scheduleEnergyLaneNextBar
  , scheduleMoodLaneNextBar
  , stepEnergyLane
  , stepMoodLane
  ) where

import Data.Word (Word32, Word64)

data AutomationCurve
  = AutomationStep
  | AutomationLinear
  | AutomationEaseInOut
  deriving (Eq, Show)

data EnergyLane = EnergyLane
  { elStartFrame ∷ !Word64
  , elEndFrame   ∷ !Word64
  , elFromEnergy ∷ !Float
  , elToEnergy   ∷ !Float
  , elCurve      ∷ !AutomationCurve
  , elStarted    ∷ !Bool
  }
  deriving (Eq, Show)

data MoodLane = MoodLane
  { mlStartFrame ∷ !Word64
  , mlEndFrame   ∷ !Word64
  , mlFromMood   ∷ !(Maybe String)
  , mlToMood     ∷ !(Maybe String)
  , mlStarted    ∷ !Bool
  }
  deriving (Eq, Show)

data EnergyLaneEvent
  = EnergyLaneStarted
      { eleStartFrame ∷ !Word64
      , eleEndFrame   ∷ !Word64
      , eleFromEnergy ∷ !Float
      , eleToEnergy   ∷ !Float
      , eleCurve      ∷ !AutomationCurve
      }
  | EnergyLaneCompleted
      { eleEndFrame ∷ !Word64
      , eleFinalEnergy ∷ !Float
      }
  deriving (Eq, Show)

data MoodLaneEvent
  = MoodLaneStarted
      { mleStartFrame ∷ !Word64
      , mleEndFrame   ∷ !Word64
      , mleFromMood   ∷ !(Maybe String)
      , mleToMood     ∷ !(Maybe String)
      }
  | MoodLaneCompleted
      { mleEndFrame ∷ !Word64
      , mleFinalMood ∷ !(Maybe String)
      }
  deriving (Eq, Show)

barFramesForTransport ∷ Word32 → Int → Float → Word64
barFramesForTransport sampleRateHz beatsPerBar bpm =
  let beats = max 1 beatsPerBar
      tempo = if isNaN bpm || isInfinite bpm then 120 else max 1 bpm
      framesPerBarD =
        (fromIntegral sampleRateHz ∷ Double)
          * 60
          * fromIntegral beats
          / realToFrac tempo
      framesPerBarI = max 1 (floor framesPerBarD ∷ Integer)
  in fromIntegral framesPerBarI

scheduleEnergyLaneNextBar
  ∷ Word64
  → Word32
  → Int
  → Float
  → Float
  → Float
  → Int
  → AutomationCurve
  → EnergyLane
scheduleEnergyLaneNextBar now sampleRateHz beatsPerBar bpm fromEnergy toEnergy bars curve =
  let barFrames = barFramesForTransport sampleRateHz beatsPerBar bpm
      start = nextBarBoundaryFrame now barFrames
      durationBars = max 0 bars
      end = start + fromIntegral durationBars * barFrames
  in EnergyLane
       { elStartFrame = start
       , elEndFrame = end
       , elFromEnergy = clamp01 fromEnergy
       , elToEnergy = clamp01 toEnergy
       , elCurve = curve
       , elStarted = False
       }

scheduleMoodLaneNextBar
  ∷ Word64
  → Word32
  → Int
  → Float
  → Maybe String
  → Maybe String
  → Int
  → MoodLane
scheduleMoodLaneNextBar now sampleRateHz beatsPerBar bpm fromMood toMood bars =
  let barFrames = barFramesForTransport sampleRateHz beatsPerBar bpm
      start = nextBarBoundaryFrame now barFrames
      durationBars = max 0 bars
      end = start + fromIntegral durationBars * barFrames
  in MoodLane
       { mlStartFrame = start
       , mlEndFrame = end
       , mlFromMood = fromMood
       , mlToMood = toMood
       , mlStarted = False
       }

stepEnergyLane
  ∷ Word64
  → EnergyLane
  → (Maybe Float, Maybe EnergyLane, [EnergyLaneEvent])
stepEnergyLane now lane
  | now < elStartFrame lane = (Nothing, Just lane, [])
  | now >= elEndFrame lane =
      let startedEvent =
            if elStarted lane
              then []
              else
                [ EnergyLaneStarted
                    (elStartFrame lane)
                    (elEndFrame lane)
                    (elFromEnergy lane)
                    (elToEnergy lane)
                    (elCurve lane)
                ]
          completedEvent =
            [EnergyLaneCompleted (elEndFrame lane) (elToEnergy lane)]
      in (Just (elToEnergy lane), Nothing, startedEvent <> completedEvent)
  | otherwise =
      let progressRaw =
            fromIntegral (now - elStartFrame lane)
              / fromIntegral (max 1 (elEndFrame lane - elStartFrame lane))
          progress = automationCurveValue (elCurve lane) progressRaw
          current =
            clamp01
              (elFromEnergy lane + (elToEnergy lane - elFromEnergy lane) * realToFrac progress)
          startedEvent =
            if elStarted lane
              then []
              else
                [ EnergyLaneStarted
                    (elStartFrame lane)
                    (elEndFrame lane)
                    (elFromEnergy lane)
                    (elToEnergy lane)
                    (elCurve lane)
                ]
      in (Just current, Just lane { elStarted = True }, startedEvent)

stepMoodLane
  ∷ Word64
  → MoodLane
  → (Maybe (Maybe String), Maybe MoodLane, [MoodLaneEvent])
stepMoodLane now lane
  | now < mlStartFrame lane = (Nothing, Just lane, [])
  | now >= mlEndFrame lane =
      let startedEvent =
            if mlStarted lane
              then []
              else [MoodLaneStarted (mlStartFrame lane) (mlEndFrame lane) (mlFromMood lane) (mlToMood lane)]
          completedEvent =
            [MoodLaneCompleted (mlEndFrame lane) (mlToMood lane)]
      in (Just (mlToMood lane), Nothing, startedEvent <> completedEvent)
  | otherwise =
      let startedEvent =
            if mlStarted lane
              then []
              else [MoodLaneStarted (mlStartFrame lane) (mlEndFrame lane) (mlFromMood lane) (mlToMood lane)]
      in (Nothing, Just lane { mlStarted = True }, startedEvent)

automationCurveValue ∷ AutomationCurve → Double → Double
automationCurveValue curve t0 =
  let t = clamp01D t0
  in case curve of
       AutomationStep -> if t < 1 then 0 else 1
       AutomationLinear -> t
       AutomationEaseInOut -> t * t * (3 - 2 * t)

nextBarBoundaryFrame ∷ Word64 → Word64 → Word64
nextBarBoundaryFrame now barFrames =
  let f = max 1 barFrames
      nowI = fromIntegral now ∷ Integer
      fI = fromIntegral f ∷ Integer
  in fromIntegral (((nowI `div` fI) + 1) * fI)

clamp01 ∷ Float → Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

clamp01D ∷ Double → Double
clamp01D x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x
