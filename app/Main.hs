{-# LANGUAGE OverloadedStrings, Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (isSuffixOf, nub, stripPrefix)
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M

import Audio.Config (audioConfigPath, loadAudioConfig)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument, gmDrumInstrument, gmProgramInstrument)
import Audio.Thread
import Audio.Types
import Player.Thread
import Player.Timeline
  ( InstrumentPatternSpec(..)
  , SectionSpec(..)
  , SongSpec(..)
  , TimelineBar(..)
  , TimelineNote(..)
  , compileTimelineBars
  , cueSectionOrder
  , lookupGeneratedInstrument
  , loadSongSpec
  )

import Midi.Play (playMidiFile, defaultChannelMap)

data CliMode
  = CliPlay !AudioHealthVerbosity !FilePath
  | CliDumpTimeline !FilePath
  | CliNotePreview !AudioHealthVerbosity

main ∷ IO ()
main = do
  args <- getArgs
  case parseCliArgs args of
    Right cliMode ->
      case cliMode of
        CliPlay healthVerbosity inputPath -> do
          audioCfg <- loadAudioConfig audioConfigPath
          bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
            bracket (startPlayerThread sys) stopPlayerThread $ \player -> do
              -- Preload instruments 0..15 (MIDI channels) with the same patch for now.
              preloadChannels sys
              runInputMode sys player inputPath
        CliDumpTimeline inputPath ->
          dumpTimelineJson inputPath
        CliNotePreview healthVerbosity -> do
          audioCfg <- loadAudioConfig audioConfigPath
          bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
            bracket (startPlayerThread sys) stopPlayerThread $ \player -> do
              preloadChannels sys
              runNotePreviewShell sys player
    Left usage -> do
      putStrLn usage
      exitSuccess

--------------------------------------------------------------------------------

parseCliArgs ∷ [String] → Either String CliMode
parseCliArgs = go AudioHealthNormal False False Nothing
  where
    go verbosity dumpTimeline notePreview inputPath [] =
      case (dumpTimeline, notePreview, inputPath) of
        (False, False, Just p) -> Right (CliPlay verbosity p)
        (True, False, Just p) -> Right (CliDumpTimeline p)
        (False, True, Nothing) -> Right (CliNotePreview verbosity)
        _ -> Left usageText
    go verbosity dumpTimeline notePreview inputPath (arg : rest) =
      case arg of
        "--dump-timeline-json" ->
          if dumpTimeline
            then Left usageText
            else go verbosity True notePreview inputPath rest
        "--note-preview-shell" ->
          if notePreview
            then Left usageText
            else go verbosity dumpTimeline True inputPath rest
        "--audio-health" ->
          go AudioHealthNormal dumpTimeline notePreview inputPath rest
        "--audio-health=off" ->
          go AudioHealthOff dumpTimeline notePreview inputPath rest
        "--audio-health=normal" ->
          go AudioHealthNormal dumpTimeline notePreview inputPath rest
        "--audio-health=verbose" ->
          go AudioHealthVerbose dumpTimeline notePreview inputPath rest
        _ ->
          case stripPrefix "--audio-health=" arg of
            Just _ ->
              Left ("Unknown audio health verbosity in " <> arg <> "\n" <> usageText)
            Nothing ->
              if take 2 arg == "--"
                then Left ("Unknown option " <> arg <> "\n" <> usageText)
                else
                  case inputPath of
                    Nothing -> go verbosity dumpTimeline notePreview (Just arg) rest
                    Just _ -> Left usageText

usageText ∷ String
usageText =
  unlines
    [ "usage: idou [--audio-health|--audio-health=off|--audio-health=normal|--audio-health=verbose] <file.mid|file.midi|file.wav|song.yaml>"
    , "   or: idou --dump-timeline-json <song.yaml>"
    , "   or: idou [--audio-health=off|--audio-health=normal|--audio-health=verbose] --note-preview-shell"
    , "  --audio-health defaults to normal and reports runtime health on anomalies."
    ]

dumpTimelineJson ∷ FilePath → IO ()
dumpTimelineJson inputPath = do
  loaded <- loadSongSpec inputPath
  case loaded of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right spec ->
      BL.putStrLn (Aeson.encode (timelinePreviewValue spec))

timelinePreviewValue ∷ SongSpec → Aeson.Value
timelinePreviewValue spec =
  let
    bars = compileTimelineBars 48000 spec
    sectionOrder = nub (cueSectionOrder spec <> M.keys (sgSections spec))
    instrumentNameMap =
      M.fromList
        [ let InstrumentId iid = ipInstrumentId inst
          in (iid, ipName inst)
        | inst <- sgInstruments spec
        ]
  in
    Aeson.object
      [ "sections"
          Aeson..=
            [ previewSectionValue instrumentNameMap spec bars sectionName
            | sectionName <- sectionOrder
            , M.member sectionName (sgSections spec)
            ]
      ]

previewSectionValue
  ∷ M.Map Int String
  → SongSpec
  → [TimelineBar]
  → String
  → Aeson.Value
previewSectionValue instrumentNameMap spec bars sectionName =
  let
    sectionSpec = sgSections spec M.! sectionName
    sectionBars = filter ((== sectionName) . tbSectionName) bars
    totalBeats =
      fromIntegral (ssBarsPerPhrase sectionSpec * ssPhraseCount sectionSpec * ssBeatsPerBar sectionSpec) ∷ Double
  in
    Aeson.object
      [ "name" Aeson..= sectionName
      , "beatsPerBar" Aeson..= ssBeatsPerBar sectionSpec
      , "totalBeats" Aeson..= totalBeats
      , "notes"
          Aeson..=
            concatMap
              (previewBarNotes instrumentNameMap sectionSpec)
              sectionBars
      ]

previewBarNotes
  ∷ M.Map Int String
  → SectionSpec
  → TimelineBar
  → [Aeson.Value]
previewBarNotes instrumentNameMap sectionSpec bar =
  let
    barIx = tbPhraseIx bar * ssBarsPerPhrase sectionSpec + tbBarInPhrase bar
    beatsPerBar = tbBeatsPerBar bar
    baseBeat = fromIntegral (barIx * beatsPerBar) ∷ Double
    framesPerBar = max 1 (tbLengthFrames bar)
    framesToBeats frames =
      (fromIntegral frames ∷ Double) * fromIntegral beatsPerBar / fromIntegral framesPerBar
  in
    map
      (previewNoteValue instrumentNameMap barIx baseBeat framesToBeats)
      (tbNotes bar)

previewNoteValue
  ∷ M.Map Int String
  → Int
  → Double
  → (Word64 → Double)
  → TimelineNote
  → Aeson.Value
previewNoteValue instrumentNameMap barIx baseBeat framesToBeats note =
  let
    InstrumentId iid = tnInstrumentId note
    NoteKey key = tnKey note
    onFrames = tnOnOffsetFrames note
    offFrames = tnOffOffsetFrames note
    durationFrames =
      if offFrames >= onFrames
        then offFrames - onFrames
        else 0
    instrumentName = M.findWithDefault ("instrument-" <> show iid) iid instrumentNameMap
  in
    Aeson.object
      [ "instrumentId" Aeson..= iid
      , "instrumentName" Aeson..= instrumentName
      , "key" Aeson..= key
      , "velocity" Aeson..= tnVelocity note
      , "amp" Aeson..= tnAmp note
      , "pan" Aeson..= tnPan note
      , "barIndex" Aeson..= barIx
      , "startBeat" Aeson..= (baseBeat + framesToBeats onFrames)
      , "durationBeats" Aeson..= framesToBeats durationFrames
      ]

runInputMode ∷ AudioSystem → PlayerSystem → FilePath → IO ()
runInputMode sys player inputPath
  | hasExt ".mid" inputPath || hasExt ".midi" inputPath = do
      playMidiInput sys inputPath
      putStrLn "Press q to quit."
      withRawStdin (waitForQuit sys)
  | hasExt ".wav" inputPath = do
      playWavInput sys inputPath
      putStrLn "Press q to quit."
      withRawStdin (waitForQuit sys)
  | hasExt ".yaml" inputPath || hasExt ".yml" inputPath =
      playSongTimelineInteractive sys player inputPath
  | otherwise =
      ioError (userError ("Unsupported input file type: " <> inputPath <> "\n" <> usageText))

playMidiInput ∷ AudioSystem → FilePath → IO ()
playMidiInput sys inputPath = do
  _ <- forkIO (playMidiFile defaultChannelMap sys inputPath)
  putStrLn ("Playing MIDI file: " <> inputPath)

playWavInput ∷ AudioSystem → FilePath → IO ()
playWavInput sys inputPath = do
  let clip = ClipId 0
  loadClipFromFile sys clip inputPath
  sendAudio sys (AudioPlayClip clip AudioBusMusic 1 0 False)
  putStrLn ("Playing WAV file: " <> inputPath)

data PromptCmd
  = PromptHelp
  | PromptQuit
  | PromptGenre !(Maybe String)
  | PromptMood !(Maybe String)
  | PromptEnergy !Float
  | PromptAutoEnergy !Float !Int !AutomationCurve
  | PromptAutoMood !(Maybe String) !Int
  | PromptCancelEnergy
  | PromptCancelMood
  | PromptTempo !Float
  | PromptMeter !Int
  | PromptStart
  | PromptStop
  | PromptPanic
  | PromptPreviewNote !String !String !Int !Float !Int
  | PromptUnknown !String

playSongTimelineInteractive ∷ AudioSystem → PlayerSystem → FilePath → IO ()
playSongTimelineInteractive sys player path = do
  instCounterRef <- newIORef 1
  sendPlayer player (PlayerLoadSongTimeline path)
  sendPlayer player PlayerStartSongTimeline
  _ <- forkIO (playerEventPrinter player)
  putStrLn ("Playing song timeline: " <> path)
  printPromptHelp
  commandLoop instCounterRef
  where
    commandLoop instCounterRef = do
      putStr "idou> "
      hFlush stdout
      eof <- hIsEOF stdin
      if eof
        then quitInteractive
        else do
          line <- getLine
          done <- runPromptCommand sys player instCounterRef (parsePromptCmd line)
          if done then pure () else commandLoop instCounterRef

runNotePreviewShell ∷ AudioSystem → PlayerSystem → IO ()
runNotePreviewShell sys player = do
  instCounterRef <- newIORef 1
  _ <- forkIO (playerEventPrinter player)
  putStrLn "[preview] note preview shell ready"
  commandLoop instCounterRef
  where
    commandLoop instCounterRef = do
      eof <- hIsEOF stdin
      if eof
        then quitInteractive
        else do
          line <- getLine
          done <- runPromptCommand sys player instCounterRef (parsePromptCmd line)
          if done then pure () else commandLoop instCounterRef

quitInteractive ∷ IO ()
quitInteractive = do
  putStrLn "Quit."
  exitSuccess

runPromptCommand ∷ AudioSystem → PlayerSystem → IORef Word64 → PromptCmd → IO Bool
runPromptCommand sys player instCounterRef cmd =
  case cmd of
    PromptHelp -> printPromptHelp >> pure False
    PromptQuit -> do
      sendPlayer player PlayerStopSongTimeline
      sendAudio sys AudioStopAll
      quitInteractive
      pure True
    PromptGenre genre -> do
      case genre of
        Nothing -> sendPlayer player PlayerClearGenreTarget
        Just g -> sendPlayer player (PlayerSetGenreTarget g)
      pure False
    PromptMood mood -> do
      case mood of
        Nothing -> sendPlayer player PlayerClearMoodTarget
        Just m -> sendPlayer player (PlayerSetMoodTarget m)
      pure False
    PromptEnergy x -> sendPlayer player (PlayerSetEnergyTarget x) >> pure False
    PromptAutoEnergy target bars curve -> do
      sendPlayer player (PlayerAutomateEnergyNextBar target bars curve)
      pure False
    PromptAutoMood mood bars -> do
      let target = maybe "" id mood
      sendPlayer player (PlayerAutomateMoodNextBar target bars)
      pure False
    PromptCancelEnergy ->
      sendPlayer player PlayerCancelEnergyAutomation >> pure False
    PromptCancelMood ->
      sendPlayer player PlayerCancelMoodAutomation >> pure False
    PromptTempo bpm ->
      sendPlayer player (PlayerSetTempoBpm bpm) >> pure False
    PromptMeter beats ->
      sendPlayer player (PlayerSetMeter beats) >> pure False
    PromptStart ->
      sendPlayer player PlayerStartSongTimeline >> pure False
    PromptStop ->
      sendPlayer player PlayerStopSongTimeline >> pure False
    PromptPanic ->
      sendAudio sys AudioStopAll >> pure False
    PromptPreviewNote genre instrumentName key velocity durationMs -> do
      previewGeneratedNote sys instCounterRef genre instrumentName key velocity durationMs
      pure False
    PromptUnknown msg -> do
      putStrLn msg
      pure False

parsePromptCmd ∷ String → PromptCmd
parsePromptCmd raw =
  let ws = words raw
      lower = map (map toLower) ws
      parseFloat s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing
      parseInt s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing
      parseCurve s =
        case map toLower s of
          "step" -> Just AutomationStep
          "linear" -> Just AutomationLinear
          "ease" -> Just AutomationEaseInOut
          "easeinout" -> Just AutomationEaseInOut
          _ -> Nothing
      parseMaybeLabel s =
        let m = trim s
            lowered = map toLower m
        in if null m || lowered == "none" || lowered == "default"
              then Nothing
              else Just m
  in
    case lower of
      [] -> PromptHelp
      ["help"] -> PromptHelp
      ["h"] -> PromptHelp
      ["q"] -> PromptQuit
      ["quit"] -> PromptQuit
      ["exit"] -> PromptQuit
      ["panic"] -> PromptPanic
      ["start"] -> PromptStart
      ["stop"] -> PromptStop
      ["note", genre, instrumentName, keyS] ->
        case parseInt keyS of
          Just key -> PromptPreviewNote genre instrumentName key 0.72 360
          Nothing -> PromptUnknown "Usage: note <genre> <instrument> <midi-key> [velocity 0..1] [duration-ms]"
      ["note", genre, instrumentName, keyS, velocityS] ->
        case (parseInt keyS, parseFloat velocityS) of
          (Just key, Just velocity) -> PromptPreviewNote genre instrumentName key velocity 360
          _ -> PromptUnknown "Usage: note <genre> <instrument> <midi-key> [velocity 0..1] [duration-ms]"
      ["note", genre, instrumentName, keyS, velocityS, durationS] ->
        case (parseInt keyS, parseFloat velocityS, parseInt durationS) of
          (Just key, Just velocity, Just durationMs) ->
            PromptPreviewNote genre instrumentName key velocity durationMs
          _ -> PromptUnknown "Usage: note <genre> <instrument> <midi-key> [velocity 0..1] [duration-ms]"
      ["cancel-energy"] -> PromptCancelEnergy
      ["cancel-mood"] -> PromptCancelMood
      ("genre" : genreParts) ->
        PromptGenre (parseMaybeLabel (unwords genreParts))
      ("mood" : moodParts) ->
        PromptMood (parseMaybeLabel (unwords moodParts))
      ["energy", x] ->
        maybe (PromptUnknown "Usage: energy <0..1>") PromptEnergy (parseFloat x)
      ["tempo", x] ->
        maybe (PromptUnknown "Usage: tempo <bpm>") PromptTempo (parseFloat x)
      ["meter", x] ->
        maybe (PromptUnknown "Usage: meter <beats-per-bar>") PromptMeter (parseInt x)
      ["auto-energy", targetS, barsS] ->
        case (parseFloat targetS, parseInt barsS) of
          (Just target, Just bars) -> PromptAutoEnergy target bars AutomationLinear
          _ -> PromptUnknown "Usage: auto-energy <target 0..1> <bars> [step|linear|ease]"
      ["auto-energy", targetS, barsS, curveS] ->
        case (parseFloat targetS, parseInt barsS, parseCurve curveS) of
          (Just target, Just bars, Just curve) -> PromptAutoEnergy target bars curve
          _ -> PromptUnknown "Usage: auto-energy <target 0..1> <bars> [step|linear|ease]"
      ("auto-mood" : barsS : moodPartsRev) ->
        case parseInt barsS of
          Nothing -> PromptUnknown "Usage: auto-mood <bars> <mood|none>"
          Just bars ->
            let moodText = unwords moodPartsRev
            in PromptAutoMood (parseMaybeLabel moodText) bars
      _ -> PromptUnknown "Unknown command. Type `help` for available commands."

printPromptHelp ∷ IO ()
printPromptHelp =
  putStrLn $
    unlines
      [ "Interactive commands:"
      , "  help | h                 Show this help"
      , "  q | quit | exit          Quit"
      , "  start                    Start timeline playback"
      , "  stop                     Stop timeline playback"
      , "  panic                    Stop all audio voices/clips immediately"
      , "  note <genre> <instrument> <midi-key> [velocity] [duration-ms]"
      , "                           Preview one generated instrument note"
      , "  genre <name|default>     Set/clear live genre override"
      , "  mood <name|none>         Set/clear mood target"
      , "  energy <0..1>            Set energy target immediately"
      , "  auto-energy <t> <bars> [step|linear|ease]"
      , "                           Ramp energy from current value starting next bar"
      , "  auto-mood <bars> <mood|none>"
      , "                           Switch mood at end of lane starting next bar"
      , "  cancel-energy            Cancel active energy automation lane"
      , "  cancel-mood              Cancel active mood automation lane"
      , "  tempo <bpm>              Set transport tempo immediately"
      , "  meter <beats>            Set beats-per-bar for next-bar quantization"
      ]

playerEventPrinter ∷ PlayerSystem → IO ()
playerEventPrinter player = loop
  where
    loop = do
      m <- tryReadPlayerEvent player
      case m of
        Nothing -> threadDelay 20000 >> loop
        Just ev -> do
          printPlayerEvent ev
          loop

printPlayerEvent ∷ PlayerEvent → IO ()
printPlayerEvent ev =
  case ev of
    PlayerEventTimelineLoaded path ->
      putStrLn ("[player] loaded timeline: " <> path)
    PlayerEventTimelineLoadFailed path err ->
      putStrLn ("[player] timeline load failed (" <> path <> "): " <> err)
    PlayerEventTimelineStarted frame bars ->
      putStrLn ("[player] timeline started at frame " <> show frame <> " (" <> show bars <> " bars)")
    PlayerEventTimelineStopped ->
      putStrLn "[player] timeline stopped"
    PlayerEventTimelineFinished ->
      putStrLn "[player] timeline finished"
    PlayerEventTimelineTransition t ->
      putStrLn
        ("[player] transition " <> show t)
    PlayerEventEnergyAutomationStarted startF endF fromE toE curve ->
      putStrLn
        ( "[player] energy lane started frame="
            <> show startF
            <> "->"
            <> show endF
            <> " "
            <> show fromE
            <> "->"
            <> show toE
            <> " curve="
            <> show curve
        )
    PlayerEventEnergyAutomationCompleted endF finalE ->
      putStrLn ("[player] energy lane completed frame=" <> show endF <> " value=" <> show finalE)
    PlayerEventMoodAutomationStarted startF endF fromM toM ->
      putStrLn
        ( "[player] mood lane started frame="
            <> show startF
            <> "->"
            <> show endF
            <> " "
            <> show fromM
            <> "->"
            <> show toM
        )
    PlayerEventMoodAutomationCompleted endF finalM ->
      putStrLn ("[player] mood lane completed frame=" <> show endF <> " value=" <> show finalM)
    PlayerEventEnergyAutomationCanceled ->
      putStrLn "[player] energy lane canceled"
    PlayerEventMoodAutomationCanceled ->
      putStrLn "[player] mood lane canceled"
    PlayerEventGenreChanged genre ->
      putStrLn ("[player] genre set to " <> genre)
    PlayerEventGenreChangeRejected requested err ->
      putStrLn ("[player] genre change rejected (" <> requested <> "): " <> err)
    PlayerEventScheduled _ _ ->
      pure ()
    PlayerEventAudio _ ->
      pure ()

hasExt ∷ String → FilePath → Bool
hasExt ext path = ext `isSuffixOf` map toLower path

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse

preloadChannels ∷ AudioSystem → IO ()
preloadChannels sys = do
  forM_ [0 .. 15] $ \ch ->
    sendAudio
      sys
      (AudioLoadInstrument (InstrumentId ch) (gmChannelInstrument ch defaultMidiProgram))

  -- Default vibrato/LFO off.
  forM_ [0 .. 15] $ \ch ->
    sendAudio sys (AudioSetVibrato (InstrumentId ch) 0 0)

previewGeneratedNote ∷ AudioSystem → IORef Word64 → String → String → Int → Float → Int → IO ()
previewGeneratedNote sys instCounterRef genre instrumentName key velocity durationMs =
  case lookupGeneratedInstrument genre instrumentName of
    Left err ->
      putStrLn ("[preview] " <> err)
    Right instrumentSpec -> do
      let normalizedKey = max 0 (min 127 key)
          velocity' = max 0 (min 1 velocity)
          durationUs = max 1 durationMs * 1000
          baseIid = ipInstrumentId instrumentSpec
          previewIid =
            if map toLower (ipName instrumentSpec) == "drums"
              then previewDrumInstrumentId normalizedKey
              else baseIid
          previewKey =
            if map toLower (ipName instrumentSpec) == "drums"
              then NoteKey 60
              else NoteKey normalizedKey
      noteInst <- nextPreviewInstanceId instCounterRef
      case map toLower (ipName instrumentSpec) of
        "drums" ->
          sendAudio sys (AudioLoadInstrument previewIid (gmDrumInstrument normalizedKey))
        _ ->
          let InstrumentId program = baseIid
          in sendAudio sys (AudioLoadInstrument previewIid (gmProgramInstrument program))
      sendAudio
        sys
        (AudioNoteOn
          { instrumentId = previewIid
          , amp = ipAmp instrumentSpec
          , pan = ipPan instrumentSpec
          , noteKey = previewKey
          , noteInstanceId = noteInst
          , velocity = velocity'
          , adsrOverride = Nothing
          })
      _ <- forkIO $ do
        threadDelay durationUs
        sendAudio sys (AudioNoteOff previewIid noteInst)
      putStrLn
        ( "[preview] note "
            <> show normalizedKey
            <> " on "
            <> ipName instrumentSpec
            <> " (genre="
            <> genre
            <> ", velocity="
            <> show velocity'
            <> ", durationMs="
            <> show (max 1 durationMs)
            <> ")"
        )

nextPreviewInstanceId ∷ IORef Word64 → IO NoteInstanceId
nextPreviewInstanceId ref =
  NoteInstanceId <$> atomicModifyIORef' ref (\n -> (n + 1, n))

previewDrumInstrumentId ∷ Int → InstrumentId
previewDrumInstrumentId key =
  InstrumentId (128 + max 0 (min 127 key))

waitForQuit ∷ AudioSystem → IO ()
waitForQuit sys = do
  let loop = do
        c <- hGetChar stdin
        case c of
          'q' -> do
            sendAudio sys AudioStopAll
            putStrLn "\nQuit."
            exitSuccess
          _ -> loop
  loop

withRawStdin ∷ IO a → IO a
withRawStdin action = do
  oldEcho <- hGetEcho stdin
  oldBuf <- hGetBuffering stdin
  bracket
    ( do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        pure (oldEcho, oldBuf)
    )
    (\(e, b) -> do hSetEcho stdin e; hSetBuffering stdin b)
    (\_ -> action)
