{-# LANGUAGE Strict, UnicodeSyntax #-}

module Player.Runtime
  ( PlayerSystem
  , PlayerEvent(..)
  , PlayerRuntimeStatus(..)
  , RuntimeChangeTiming(..)
  , startRuntime
  , stopRuntime
  , tryReadEvent
  , requestStatus
  , loadSong
  , loadClipFile
  , startSongNextBar
  , stopSong
  , pausePlayback
  , resumePlayback
  , setGenreTarget
  , clearGenreTarget
  , setMoodTarget
  , clearMoodTarget
  , setEnergyTarget
  , setTempoBpm
  , setMeter
  , automateEnergyNextBar
  , automateMoodNextBar
  , cancelEnergyAutomation
  , cancelMoodAutomation
  , playMusicClipNow
  , playSfxClipNow
  , scheduleMusicClipNextBar
  , scheduleSfxClipNextBar
  , scheduleBusGainNextBar
  , scheduleNoteOnBusNextBar
  , scheduleNoteOnNextBar
  , scheduleNoteOffNextBar
  , playClipNow
  , stopClipNow
  , setBusGainNow
  , stopBusNow
  , noteOnBusNow
  , noteOnNow
  , noteOffNow
  , panic
  , readTransportFrame
  , nextBarBoundaryFrame
  ) where

import Data.Word (Word64)

import Audio.Envelope (ADSR)
import Audio.Thread (AudioSystem(..), readTransportFrame)
import Audio.Types
import Player.Thread
  ( AutomationCurve
  , PlayerEvent(..)
  , PlayerRuntimeStatus(..)
  , PlayerSystem
  , automatePlayerEnergyNextBar
  , automatePlayerMoodNextBar
  , cancelPlayerEnergyAutomation
  , cancelPlayerMoodAutomation
  , clearPlayerGenreTarget
  , clearPlayerMoodTarget
  , loadPlayerClip
  , loadPlayerSongTimeline
  , nextBarFrame
  , panicPlayer
  , pausePlayer
  , playerNoteOffNow
  , playerNoteOnNow
  , playPlayerClipNow
  , requestPlayerStatus
  , resumePlayer
  , schedulePlayerBusGainNextBar
  , schedulePlayerClipNextBar
  , schedulePlayerNoteOffNextBar
  , schedulePlayerNoteOnNextBar
  , setPlayerBusGainNow
  , setPlayerEnergyTarget
  , setPlayerGenreTarget
  , setPlayerMeter
  , setPlayerMoodTarget
  , setPlayerTempoBpm
  , startPlayerSongTimeline
  , startPlayerThread
  , stopPlayerBusNow
  , stopPlayerClipNow
  , stopPlayerSongTimeline
  , stopPlayerThread
  , tryReadPlayerEvent
  )

-- | Timing buckets for the intended game-facing control surface.
data RuntimeChangeTiming
  = RuntimeImmediate
  | RuntimeNextBar
  | RuntimeFutureLookahead
  deriving (Eq, Show)

startRuntime ∷ AudioSystem → IO PlayerSystem
startRuntime = startPlayerThread

stopRuntime ∷ PlayerSystem → IO ()
stopRuntime = stopPlayerThread

tryReadEvent ∷ PlayerSystem → IO (Maybe PlayerEvent)
tryReadEvent = tryReadPlayerEvent

requestStatus ∷ PlayerSystem → IO ()
requestStatus = requestPlayerStatus

-- | Immediate: stages a song spec for later playback.
loadSong ∷ PlayerSystem → FilePath → IO ()
loadSong = loadPlayerSongTimeline

-- | Immediate: decode and stage a clip asset for later playback.
loadClipFile ∷ PlayerSystem → ClipId → FilePath → IO ()
loadClipFile = loadPlayerClip

-- | Next bar: begins the staged adaptive song on the next transport boundary.
startSongNextBar ∷ PlayerSystem → IO ()
startSongNextBar = startPlayerSongTimeline

-- | Immediate: stops the adaptive song timeline and clears active voices.
stopSong ∷ PlayerSystem → IO ()
stopSong = stopPlayerSongTimeline

-- | Immediate: pauses the player thread.
pausePlayback ∷ PlayerSystem → IO ()
pausePlayback = pausePlayer

-- | Immediate: resumes the player thread.
resumePlayback ∷ PlayerSystem → IO ()
resumePlayback = resumePlayer

-- | Immediate: sets a live genre override for future adaptive generation.
setGenreTarget ∷ PlayerSystem → String → IO ()
setGenreTarget = setPlayerGenreTarget

-- | Immediate: clears the live genre override.
clearGenreTarget ∷ PlayerSystem → IO ()
clearGenreTarget = clearPlayerGenreTarget

-- | Immediate: retargets future adaptive generation toward a mood.
setMoodTarget ∷ PlayerSystem → String → IO ()
setMoodTarget = setPlayerMoodTarget

-- | Immediate: clears the active mood target.
clearMoodTarget ∷ PlayerSystem → IO ()
clearMoodTarget = clearPlayerMoodTarget

-- | Immediate: retargets future adaptive generation toward an energy value.
setEnergyTarget ∷ PlayerSystem → Float → IO ()
setEnergyTarget = setPlayerEnergyTarget

-- | Immediate: updates transport tempo.
setTempoBpm ∷ PlayerSystem → Float → IO ()
setTempoBpm = setPlayerTempoBpm

-- | Immediate: updates beats-per-bar used for next-bar quantization.
setMeter ∷ PlayerSystem → Int → IO ()
setMeter = setPlayerMeter

-- | Next bar: starts an energy automation lane aligned to the transport.
automateEnergyNextBar ∷ PlayerSystem → Float → Int → AutomationCurve → IO ()
automateEnergyNextBar = automatePlayerEnergyNextBar

-- | Next bar: starts a mood automation lane aligned to the transport.
automateMoodNextBar ∷ PlayerSystem → String → Int → IO ()
automateMoodNextBar = automatePlayerMoodNextBar

-- | Immediate: cancels any active energy automation lane.
cancelEnergyAutomation ∷ PlayerSystem → IO ()
cancelEnergyAutomation = cancelPlayerEnergyAutomation

-- | Immediate: cancels any active mood automation lane.
cancelMoodAutomation ∷ PlayerSystem → IO ()
cancelMoodAutomation = cancelPlayerMoodAutomation

-- | Immediate: plays a clip on the music bus.
playMusicClipNow ∷ PlayerSystem → ClipId → Float → Float → Bool → IO ()
playMusicClipNow player clipId gain pan loop =
  playClipNow player clipId AudioBusMusic gain pan loop

-- | Immediate: plays a clip on the SFX bus.
playSfxClipNow ∷ PlayerSystem → ClipId → Float → Float → Bool → IO ()
playSfxClipNow player clipId gain pan loop =
  playClipNow player clipId AudioBusSfx gain pan loop

-- | Next bar: schedules a clip onto the music bus.
scheduleMusicClipNextBar ∷ PlayerSystem → ClipId → Float → Float → Bool → IO ()
scheduleMusicClipNextBar player clipId gain pan loop =
  schedulePlayerClipNextBar player clipId AudioBusMusic gain pan loop

-- | Next bar: schedules a clip onto the SFX bus.
scheduleSfxClipNextBar ∷ PlayerSystem → ClipId → Float → Float → Bool → IO ()
scheduleSfxClipNextBar player clipId gain pan loop =
  schedulePlayerClipNextBar player clipId AudioBusSfx gain pan loop

-- | Next bar: schedules a bus gain change in transport sync.
scheduleBusGainNextBar ∷ PlayerSystem → AudioBus → Float → IO ()
scheduleBusGainNextBar = schedulePlayerBusGainNextBar

-- | Next bar: schedules an instrument note-on in transport sync.
scheduleNoteOnBusNextBar
  ∷ PlayerSystem
  → AudioBus
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
scheduleNoteOnBusNextBar = schedulePlayerNoteOnNextBar

-- | Next bar: schedules a one-shot note on the SFX bus.
scheduleNoteOnNextBar
  ∷ PlayerSystem
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
scheduleNoteOnNextBar player =
  scheduleNoteOnBusNextBar player AudioBusSfx

-- | Next bar: schedules a note-off in transport sync.
scheduleNoteOffNextBar ∷ PlayerSystem → InstrumentId → NoteInstanceId → IO ()
scheduleNoteOffNextBar = schedulePlayerNoteOffNextBar

-- | Immediate: plays a clip on the requested bus.
playClipNow ∷ PlayerSystem → ClipId → AudioBus → Float → Float → Bool → IO ()
playClipNow = playPlayerClipNow

-- | Immediate: stops a playing clip.
stopClipNow ∷ PlayerSystem → ClipId → IO ()
stopClipNow = stopPlayerClipNow

-- | Immediate: sets a bus gain without waiting for a transport boundary.
setBusGainNow ∷ PlayerSystem → AudioBus → Float → IO ()
setBusGainNow = setPlayerBusGainNow

-- | Immediate: stops all sources on a bus.
stopBusNow ∷ PlayerSystem → AudioBus → IO ()
stopBusNow = stopPlayerBusNow

-- | Immediate: starts a note directly on the audio thread.
noteOnBusNow
  ∷ PlayerSystem
  → AudioBus
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
noteOnBusNow = playerNoteOnNow

-- | Immediate: starts a one-shot note on the SFX bus.
noteOnNow
  ∷ PlayerSystem
  → InstrumentId
  → Float
  → Float
  → NoteKey
  → NoteInstanceId
  → Float
  → Maybe ADSR
  → IO ()
noteOnNow player =
  noteOnBusNow player AudioBusSfx

-- | Immediate: releases a note directly on the audio thread.
noteOffNow ∷ PlayerSystem → InstrumentId → NoteInstanceId → IO ()
noteOffNow = playerNoteOffNow

-- | Immediate: hard stop for all audio sources.
panic ∷ PlayerSystem → IO ()
panic = panicPlayer

-- | Computes the next bar boundary from the current transport frame.
nextBarBoundaryFrame ∷ AudioSystem → Int → Float → IO Word64
nextBarBoundaryFrame audioSys beatsPerBar bpm = do
  now <- readTransportFrame audioSys
  pure (nextBarFrame now (sampleRate (asHandle audioSys)) beatsPerBar bpm)
