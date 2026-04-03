{-# LANGUAGE OverloadedStrings, Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (intercalate, isSuffixOf, nub, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M

import Audio.Config (AudioConfig(..), audioConfigPath, loadAudioConfigEither)
import Audio.Patch.Loader (loadInstrumentPatchEither)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument, gmDrumInstrument)
import Audio.Thread
import Audio.Types
import Perf.Baseline
  ( BaselineScenario(..)
  , BaselineScenarioAction(..)
  , LookaheadPerfSummary(..)
  , ScheduledBaselineAction(..)
  , baselineScenarioNames
  , defaultBaselineScenario
  , lookaheadAverageBarsPerSecond
  , lookaheadAverageGenerationUs
  , lookupBaselineScenario
  , scheduleBaselineScenario
  , summarizeLookaheadTelemetry
  , timelineAbsoluteBarStartFrames
  , timelineAbsoluteEndFrameForBars
  )
import Player.Instrument (loadResolvedInstrumentEither, usesBuiltInDrumPatch)
import Player.Automation (AutomationCurve(..))
import qualified Player.Runtime as Runtime
import Player.Timeline.Explain
  ( TimelineExplainEvent(..)
  , barInstrumentNoteCounts
  , drainTimelineExplainEvents
  )
import Player.Timeline
  ( InstrumentPatternSpec(..)
  , SectionSpec(..)
  , SongSpec(..)
  , TimelineBar(..)
   , TimelineLookaheadTelemetry(..)
   , TimelineNote(..)
   , TimelineRetargetTelemetry(..)
   , TimelineTransitionTelemetry(..)
   , compileSectionNotes
   , cueSectionOrder
   , lookupGeneratedInstrument
   , loadSongSpec
  , sectionBarFrames
  )

import Midi.Play (defaultChannelMap, loadMidiPlaybackPlan, playMidiPlaybackPlan)

data CliMode
  = CliPlay !AudioHealthVerbosity !FilePath
  | CliDumpTimeline !FilePath !(Maybe Word64)
  | CliExplainTimeline !FilePath !(Maybe Word64)
  | CliBaseline !AudioHealthVerbosity !FilePath !BaselineScenario !Int
  | CliNotePreview !AudioHealthVerbosity
  | CliCheckPatch !FilePath

main ∷ IO ()
main = do
  args <- getArgs
  case parseCliArgs args of
    Right cliMode ->
      case cliMode of
        CliPlay healthVerbosity inputPath -> do
          audioCfg <- loadAudioConfigOrExit
          bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
            bracket (Runtime.startRuntime sys) Runtime.stopRuntime $ \player -> do
              -- Preload instruments 0..15 (MIDI channels) with the same patch for now.
              preloadChannels sys
              runInputMode sys player inputPath
        CliDumpTimeline inputPath mSeed ->
          dumpTimelineJson inputPath mSeed
        CliExplainTimeline inputPath mSeed ->
          explainTimeline inputPath mSeed
        CliBaseline healthVerbosity inputPath scenario barCount ->
          runBaselineReport healthVerbosity inputPath scenario barCount
        CliNotePreview healthVerbosity -> do
          audioCfg <- loadAudioConfigOrExit
          bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
            bracket (Runtime.startRuntime sys) Runtime.stopRuntime $ \player -> do
              preloadChannels sys
              runNotePreviewShell sys player
        CliCheckPatch patchPath ->
          checkPatchFile patchPath
    Left usage -> do
      putStrLn usage
      exitSuccess

--------------------------------------------------------------------------------

loadAudioConfigOrExit ∷ IO AudioConfig
loadAudioConfigOrExit = do
  result <- loadAudioConfigEither audioConfigPath
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right cfg ->
      pure cfg

parseCliArgs ∷ [String] → Either String CliMode
parseCliArgs = go AudioHealthNormal False False False False False Nothing Nothing Nothing Nothing
  where
    go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario [] =
      case (dumpTimeline, explainMode, notePreview, checkPatch, baselineMode, inputPath, timelineSeed, baselineBars) of
        (False, False, False, False, False, Just p, Nothing, Nothing) -> Right (CliPlay verbosity p)
        (True, False, False, False, False, Just p, mSeed, Nothing) -> Right (CliDumpTimeline p mSeed)
        (False, True, False, False, False, Just p, mSeed, Nothing) -> Right (CliExplainTimeline p mSeed)
        (False, False, True, False, False, Nothing, Nothing, Nothing) -> Right (CliNotePreview verbosity)
        (False, False, False, True, False, Just p, Nothing, Nothing) -> Right (CliCheckPatch p)
        (False, False, False, False, True, Just p, Nothing, mBars) ->
          let scenario = fromMaybe defaultBaselineScenario baselineScenario
          in Right (CliBaseline verbosity p scenario (maybe (bsDefaultBars scenario) id mBars))
        _ -> Left usageText
    go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario (arg : rest) =
      case arg of
        "--dump-timeline-json" ->
          if dumpTimeline || explainMode || baselineMode
            then Left usageText
            else go verbosity True False notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--explain-timeline" ->
          if dumpTimeline || explainMode || baselineMode
            then Left usageText
            else go verbosity False True notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--baseline-report" ->
          if dumpTimeline || explainMode || baselineMode
            then Left usageText
            else go verbosity False False notePreview checkPatch True inputPath timelineSeed baselineBars baselineScenario rest
        "--baseline-scenario" ->
          case rest of
            scenarioRaw : remaining ->
              case parseBaselineScenario scenarioRaw of
                Left err -> Left (err <> "\n" <> usageText)
                Right scenario ->
                  case baselineScenario of
                    Just _ -> Left usageText
                    Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars (Just scenario) remaining
            [] -> Left usageText
        "--baseline-bars" ->
          case rest of
            barsRaw : remaining ->
              case parseBaselineBars barsRaw of
                Left err -> Left (err <> "\n" <> usageText)
                Right bars ->
                  case baselineBars of
                    Just _ -> Left usageText
                    Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed (Just bars) baselineScenario remaining
            [] -> Left usageText
        "--timeline-seed" ->
          case rest of
            seedRaw : remaining ->
              case parseTimelineSeed seedRaw of
                Left err -> Left (err <> "\n" <> usageText)
                Right seed ->
                  case timelineSeed of
                    Just _ -> Left usageText
                    Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath (Just seed) baselineBars baselineScenario remaining
            [] -> Left usageText
        "--note-preview-shell" ->
          if notePreview
            then Left usageText
            else go verbosity dumpTimeline explainMode True checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--check-patch" ->
          if checkPatch
            then Left usageText
            else go verbosity dumpTimeline explainMode notePreview True baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--audio-health" ->
          go AudioHealthNormal dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--audio-health=off" ->
          go AudioHealthOff dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--audio-health=normal" ->
          go AudioHealthNormal dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        "--audio-health=verbose" ->
          go AudioHealthVerbose dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars baselineScenario rest
        _ ->
          case stripPrefix "--timeline-seed=" arg of
            Just seedRaw ->
              case parseTimelineSeed seedRaw of
                Left err -> Left (err <> "\n" <> usageText)
                Right seed ->
                  case timelineSeed of
                    Just _ -> Left usageText
                    Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath (Just seed) baselineBars baselineScenario rest
            Nothing ->
              case stripPrefix "--baseline-scenario=" arg of
                Just scenarioRaw ->
                  case parseBaselineScenario scenarioRaw of
                    Left err -> Left (err <> "\n" <> usageText)
                    Right scenario ->
                      case baselineScenario of
                        Just _ -> Left usageText
                        Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed baselineBars (Just scenario) rest
                Nothing ->
                  case stripPrefix "--baseline-bars=" arg of
                    Just barsRaw ->
                      case parseBaselineBars barsRaw of
                        Left err -> Left (err <> "\n" <> usageText)
                        Right bars ->
                          case baselineBars of
                            Just _ -> Left usageText
                            Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode inputPath timelineSeed (Just bars) baselineScenario rest
                    Nothing ->
                      case stripPrefix "--audio-health=" arg of
                        Just _ ->
                          Left ("Unknown audio health verbosity in " <> arg <> "\n" <> usageText)
                        Nothing ->
                          if take 2 arg == "--"
                            then Left ("Unknown option " <> arg <> "\n" <> usageText)
                            else
                              case inputPath of
                                Nothing -> go verbosity dumpTimeline explainMode notePreview checkPatch baselineMode (Just arg) timelineSeed baselineBars baselineScenario rest
                                Just _ -> Left usageText

usageText ∷ String
usageText =
  unlines
    [ "usage: idou [--audio-health|--audio-health=off|--audio-health=normal|--audio-health=verbose] <file.mid|file.midi|file.wav|song.yaml>"
    , "   or: idou --dump-timeline-json [--timeline-seed <seed>] <song.yaml>"
    , "   or: idou --explain-timeline [--timeline-seed <start-frame>] <song.yaml>"
    , "   or: idou [--audio-health=off|--audio-health=normal|--audio-health=verbose] --baseline-report [--baseline-scenario <name>] [--baseline-bars <count>] <song.yaml>"
    , "   or: idou [--audio-health=off|--audio-health=normal|--audio-health=verbose] --note-preview-shell"
    , "   or: idou --check-patch <patch.yaml>"
    , "  --audio-health defaults to normal and reports runtime health on anomalies."
    , "  --baseline-bars defaults to 16 and measures a fixed number of timeline bars."
    , "  baseline scenarios: " <> intercalate ", " baselineScenarioNames
    ]

parseTimelineSeed ∷ String → Either String Word64
parseTimelineSeed raw =
  case readMaybe raw of
    Just seed -> Right seed
    Nothing -> Left ("Invalid timeline seed " <> raw)

parseBaselineBars ∷ String → Either String Int
parseBaselineBars raw =
  case readMaybe raw of
    Just bars | bars > 0 -> Right bars
    _ -> Left ("Invalid baseline bar count " <> raw)

parseBaselineScenario ∷ String → Either String BaselineScenario
parseBaselineScenario raw =
  case lookupBaselineScenario raw of
    Just scenario -> Right scenario
    Nothing -> Left ("Unknown baseline scenario " <> raw)

checkPatchFile ∷ FilePath → IO ()
checkPatchFile patchPath = do
  result <- loadInstrumentPatchEither patchPath
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right _ ->
      putStrLn ("Patch OK: " <> patchPath)

dumpTimelineJson ∷ FilePath → Maybe Word64 → IO ()
dumpTimelineJson inputPath mSeed = do
  loaded <- loadSongSpec inputPath
  case loaded of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right spec ->
      BL.putStrLn (Aeson.encode (timelinePreviewValue mSeed spec))

explainTimeline ∷ FilePath → Maybe Word64 → IO ()
explainTimeline inputPath mSeed = do
  loaded <- loadSongSpec inputPath
  case loaded of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right spec ->
      case timelineExplainLines inputPath (fromMaybe 0 mSeed) spec of
        Left err -> do
          hPutStrLn stderr ("Timeline explain failed: " <> err)
          exitFailure
        Right lns -> mapM_ putStrLn lns

data BaselineRunReport = BaselineRunReport
  { brrScenario        ∷ !BaselineScenario
  , brrRequestedBars   ∷ !Int
  , brrStartFrame      ∷ !Word64
  , brrTargetFrame     ∷ !Word64
  , brrEndFrame        ∷ !Word64
  , brrTimelineFinished ∷ !Bool
  , brrScheduledActions ∷ !Int
  , brrAppliedActions  ∷ !Int
  , brrLookaheadSummary ∷ !LookaheadPerfSummary
  , brrAudioHealth     ∷ !AudioHealthSnapshot
  }

runBaselineReport ∷ AudioHealthVerbosity → FilePath → BaselineScenario → Int → IO ()
runBaselineReport healthVerbosity inputPath scenario barCount
  | not (hasExt ".yaml" inputPath || hasExt ".yml" inputPath) = do
      hPutStrLn stderr ("Baseline report expects a song YAML file: " <> inputPath)
      exitFailure
  | otherwise = do
      audioCfg <- loadAudioConfigOrExit
      loaded <- loadSongSpec inputPath
      case loaded of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right spec ->
          bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
            bracket (Runtime.startRuntime sys) Runtime.stopRuntime $ \player -> do
              loadResult <- Runtime.loadSongChecked player inputPath
              case loadResult of
                Left err -> do
                  hPutStrLn stderr ("Failed to load song timeline: " <> err)
                  exitFailure
                Right () -> pure ()
              startResult <- Runtime.startSongNextBarChecked player
              case startResult of
                Left err -> do
                  hPutStrLn stderr ("Failed to start song timeline: " <> err)
                  exitFailure
                Right () -> pure ()
              startFrame <- awaitBaselineStartFrame player
              case
                ( timelineAbsoluteBarStartFrames (acSampleRate audioCfg) startFrame barCount spec
                , timelineAbsoluteEndFrameForBars (acSampleRate audioCfg) startFrame barCount spec
                ) of
                (Nothing, _) -> do
                  hPutStrLn stderr ("Baseline report could not find " <> show barCount <> " bars in " <> inputPath)
                  exitFailure
                (_, Nothing) -> do
                  hPutStrLn stderr ("Baseline report could not find an end frame for " <> show barCount <> " bars in " <> inputPath)
                  exitFailure
                (Just _startFrames, Just targetFrame) -> do
                  let scheduledActions =
                        scheduleBaselineScenario
                          (acSampleRate audioCfg)
                          startFrame
                          barCount
                          spec
                          scenario
                  report <- collectBaselineRunReport sys player scenario barCount startFrame targetFrame scheduledActions
                  Runtime.stopSong player
                  Runtime.panic player
                  mapM_ putStrLn (renderBaselineReportLines inputPath report)

awaitBaselineStartFrame ∷ Runtime.PlayerSystem → IO Word64
awaitBaselineStartFrame player = do
  mEvent <- Runtime.tryReadEvent player
  case mEvent of
    Nothing -> threadDelay 1000 >> awaitBaselineStartFrame player
    Just event ->
      case event of
        Runtime.PlayerEventTimelineStarted frame _ ->
          pure frame
        Runtime.PlayerEventTimelineLoadFailed path err -> do
          hPutStrLn stderr ("Failed to load timeline during baseline run (" <> path <> "): " <> err)
          exitFailure
        _ ->
          awaitBaselineStartFrame player

collectBaselineRunReport
  ∷ AudioSystem
  → Runtime.PlayerSystem
  → BaselineScenario
  → Int
  → Word64
  → Word64
  → [ScheduledBaselineAction]
  → IO BaselineRunReport
collectBaselineRunReport sys player scenario requestedBars startFrame targetFrame scheduledActions =
  loop [] False scheduledActions 0
  where
    loop acc finished pendingActions appliedActions = do
      (acc', finished') <- drainPlayerEvents acc finished
      now <- readTransportFrame sys
      (pendingActions', appliedActions') <- applyDueScenarioActions now pendingActions appliedActions
      if finished' || now >= targetFrame
        then do
          threadDelay 10000
          (acc'', finished'') <- drainPlayerEvents acc' finished'
          endFrame <- readTransportFrame sys
          audioHealth <- readAudioHealth sys
          pure
            BaselineRunReport
              { brrScenario = scenario
              , brrRequestedBars = requestedBars
              , brrStartFrame = startFrame
              , brrTargetFrame = targetFrame
              , brrEndFrame = endFrame
              , brrTimelineFinished = finished''
              , brrScheduledActions = length scheduledActions
              , brrAppliedActions = appliedActions'
              , brrLookaheadSummary = summarizeLookaheadTelemetry (reverse acc'')
              , brrAudioHealth = audioHealth
              }
        else do
          threadDelay 1000
          loop acc' finished' pendingActions' appliedActions'

    drainPlayerEvents acc finished = do
      mEvent <- Runtime.tryReadEvent player
      case mEvent of
        Nothing -> pure (acc, finished)
        Just event ->
          case event of
            Runtime.PlayerEventTimelineLookahead telemetry ->
              drainPlayerEvents (telemetry : acc) finished
            Runtime.PlayerEventTimelineFinished ->
              drainPlayerEvents acc True
            Runtime.PlayerEventTimelineLoadFailed path err -> do
              hPutStrLn stderr ("Failed during baseline run (" <> path <> "): " <> err)
              exitFailure
            _ ->
              drainPlayerEvents acc finished

    applyDueScenarioActions now pendingActions appliedActions =
      let (dueActions, rest) = span (\scheduled -> sbaFrame scheduled <= now) pendingActions
      in do
        mapM_ (applyScenarioAction player . sbaAction) dueActions
        pure (rest, appliedActions + length dueActions)

renderBaselineReportLines ∷ FilePath → BaselineRunReport → [String]
renderBaselineReportLines inputPath report =
  let lookahead = brrLookaheadSummary report
      audio = brrAudioHealth report
      avgMixUs =
        if ahsRenderPassCount audio <= 0
          then 0
          else ahsMixDurationUs audio `div` ahsRenderPassCount audio
      avgGenUsText = maybe "n/a" show (lookaheadAverageGenerationUs lookahead)
      avgBarsPerSecondText = maybe "n/a" formatOneDecimal (lookaheadAverageBarsPerSecond lookahead)
      minBarsPerSecondText = maybe "n/a" formatOneDecimal (lpsMinBarsPerSecond lookahead)
      peakBarsPerSecondText =
        if lpsMeasuredEventCount lookahead <= 0
          then "n/a"
          else formatOneDecimal (lpsPeakBarsPerSecond lookahead)
  in
    [ "Performance baseline: " <> inputPath
    , "  scenario="
        <> bsName (brrScenario report)
        <> " bars="
        <> show (brrRequestedBars report)
        <> " start-frame="
        <> show (brrStartFrame report)
        <> " target-frame="
        <> show (brrTargetFrame report)
        <> " end-frame="
        <> show (brrEndFrame report)
        <> " finished="
        <> show (brrTimelineFinished report)
    , "  description=" <> bsDescription (brrScenario report)
    , "  scenario-actions="
        <> show (brrAppliedActions report)
        <> "/"
        <> show (brrScheduledActions report)
        <> " applied"
    , "  lookahead-events="
        <> show (lpsEventCount lookahead)
        <> " generated-bars="
        <> show (lpsGeneratedBars lookahead)
        <> " measured-events="
        <> show (lpsMeasuredEventCount lookahead)
        <> " reasons="
        <> formatReasonCounts (lpsReasonCounts lookahead)
    , "  lookahead-gen-us(avg/peak)="
        <> avgGenUsText
        <> "/"
        <> show (lpsPeakGenerationUs lookahead)
        <> " bars/s(avg/min/peak)="
        <> avgBarsPerSecondText
        <> "/"
        <> minBarsPerSecondText
        <> "/"
        <> peakBarsPerSecondText
    , "  audio-underruns="
        <> show (ahsUnderruns audio)
        <> " partialWrites="
        <> show (ahsPartialWrites audio)
        <> " zeroWrites="
        <> show (ahsZeroWrites audio)
        <> " renderPasses="
        <> show (ahsRenderPassCount audio)
    , "  audio-mix-us(avg/peak)="
        <> show avgMixUs
        <> "/"
        <> show (ahsPeakMixDurationUs audio)
        <> " peakVoices="
        <> show (ahsPeakActiveVoices audio)
        <> " peakClips="
        <> show (ahsPeakActiveClips audio)
        <> " mixed="
        <> show (ahsFramesMixed audio)
        <> " written="
        <> show (ahsFramesWritten audio)
    ]

applyScenarioAction ∷ Runtime.PlayerSystem → BaselineScenarioAction → IO ()
applyScenarioAction player action =
  case action of
    BaselineSetGenre Nothing ->
      Runtime.clearGenreTarget player
    BaselineSetGenre (Just genre) ->
      Runtime.setGenreTarget player genre
    BaselineSetMood Nothing ->
      Runtime.clearMoodTarget player
    BaselineSetMood (Just mood) ->
      Runtime.setMoodTarget player mood
    BaselineSetEnergy energy ->
      Runtime.setEnergyTarget player energy

timelinePreviewValue ∷ Maybe Word64 → SongSpec → Aeson.Value
timelinePreviewValue mSeed spec =
  let
    bars = compileTimelineBarsWithSeedOffset 48000 (previewSeedBarOffset mSeed) spec
    sectionOrder = nub (cueSectionOrder spec <> M.keys (sgSections spec))
    instrumentNameMap =
      M.fromList
        [ let InstrumentId iid = ipInstrumentId inst
          in (iid, ipName inst)
        | inst <- sgInstruments spec
        ]
  in
    Aeson.object
      [ "seed" Aeson..= fromMaybe 0 mSeed
      , "sections"
          Aeson..=
            [ previewSectionValue instrumentNameMap spec bars sectionName
            | sectionName <- sectionOrder
            , M.member sectionName (sgSections spec)
            ]
      ]

previewSeedBarOffset ∷ Maybe Word64 → Int
previewSeedBarOffset =
  fromIntegral . (`mod` 1048576) . fromMaybe 0

normalizedPreviewMood ∷ SongSpec → Maybe String
normalizedPreviewMood spec =
  let stripped = trimSpaces (sgMood spec)
  in if null stripped then Nothing else Just (map toLower stripped)

trimSpaces ∷ String → String
trimSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

compileTimelineBarsWithSeedOffset ∷ Word32 → Int → SongSpec → [TimelineBar]
compileTimelineBarsWithSeedOffset sampleRateHz seedOffset spec = reverse barsRev
  where
    (barsRev, _offset, _absIx) =
      foldl
        compileSection
        ([], 0, seedOffset)
        (cueSectionOrder spec)

    compileSection (acc, offsetFrames, absIx) sectionName =
      case M.lookup sectionName (sgSections spec) of
        Nothing -> (acc, offsetFrames, absIx)
        Just sectionSpec ->
          let barsPerPhrase = max 1 (ssBarsPerPhrase sectionSpec)
              phraseCount = max 1 (ssPhraseCount sectionSpec)
              barCount = barsPerPhrase * phraseCount
              barFrames = sectionBarFrames sampleRateHz sectionSpec
              buildBar i =
                let i64 = fromIntegral i ∷ Word64
                    absIx' = absIx + i
                    notes =
                      compileSectionNotes
                        sampleRateHz
                        (sgGenre spec)
                        sectionName
                        sectionSpec
                        (normalizedPreviewMood spec)
                        0.5
                        Nothing
                        absIx'
                        (i `mod` barsPerPhrase)
                        barsPerPhrase
                        (sgInstruments spec)
                in TimelineBar
                    { tbSectionName = sectionName
                    , tbAbsoluteBarIx = absIx'
                    , tbPhraseIx = i `div` barsPerPhrase
                    , tbBarInPhrase = i `mod` barsPerPhrase
                    , tbTempoBpm = ssTempoBpm sectionSpec
                    , tbBeatsPerBar = ssBeatsPerBar sectionSpec
                    , tbBeatUnit = ssBeatUnit sectionSpec
                    , tbStartOffsetFrames = offsetFrames + i64 * barFrames
                    , tbLengthFrames = barFrames
                    , tbNotes = notes
                    }
              newBars = map buildBar [0 .. barCount - 1]
              nextOffset = offsetFrames + fromIntegral barCount * barFrames
          in (reverse newBars <> acc, nextOffset, absIx + barCount)

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

timelineExplainLines ∷ FilePath → Word64 → SongSpec → Either String [String]
timelineExplainLines inputPath startFrame spec =
  let instrumentNameMap =
        M.fromList
          [ let InstrumentId iid = ipInstrumentId inst
            in (iid, ipName inst)
          | inst <- sgInstruments spec
          ]
      header =
        [ "Timeline explain: " <> inputPath
        , "  start-frame=" <> show startFrame
            <> " genre="
            <> sgGenre spec
            <> " mood="
            <> showMaybeLabel (normalizedPreviewMood spec)
            <> " lookahead-bars="
            <> show (sgLookaheadBars spec)
        ]
  in case drainTimelineExplainEvents 48000 startFrame spec of
       Left err -> Left err
       Right events ->
         let body = concatMap (\ev -> renderTimelineExplainEventLines instrumentNameMap startFrame ev <> [""]) events
         in Right (header <> [""] <> dropTrailingBlank body)

renderTimelineExplainEventLines
  ∷ M.Map Int String
  → Word64
  → TimelineExplainEvent
  → [String]
renderTimelineExplainEventLines instrumentNameMap startFrame ev =
  case ev of
    TimelineExplainRetarget t ->
      renderRetargetTelemetryLines "[explain]" t
    TimelineExplainLookahead t ->
      renderLookaheadTelemetryLines "[explain]" t
    TimelineExplainTransition t ->
      renderTransitionTelemetryLines "[explain]" t
    TimelineExplainBar bar ->
      let absStart = startFrame + tbStartOffsetFrames bar
      in
        [ "[explain] bar "
            <> show (tbAbsoluteBarIx bar)
            <> " section="
            <> tbSectionName bar
            <> " phrase="
            <> show (tbPhraseIx bar)
            <> " bar-in-phrase="
            <> show (tbBarInPhrase bar)
        , "[explain]   start-frame="
            <> show absStart
            <> " length="
            <> show (tbLengthFrames bar)
            <> " notes="
            <> show (length (tbNotes bar))
            <> " instruments="
            <> formatInstrumentCounts instrumentNameMap (barInstrumentNoteCounts bar)
        ]

renderRetargetTelemetryLines ∷ String → TimelineRetargetTelemetry → [String]
renderRetargetTelemetryLines prefix t =
  [ prefix
      <> " retarget reason="
      <> trtReason t
      <> " policy="
      <> trtPolicy t
  , prefix
      <> "   buffered-future-bars="
      <> show (trtBufferedFutureBars t)
      <> " next-generated-bar="
      <> show (trtNextGeneratedBarIx t)
      <> " next-frame="
      <> show (trtNextGeneratedFrame t)
      <> " targets: genre="
      <> trtGenre t
      <> " mood="
      <> showMaybeLabel (trtMoodTarget t)
      <> " energy="
      <> show (trtEnergyTarget t)
  ]

renderLookaheadTelemetryLines ∷ String → TimelineLookaheadTelemetry → [String]
renderLookaheadTelemetryLines prefix t =
  let perfSuffix =
        case tltGenerationDurationUs t of
          Nothing -> ""
          Just durationUs ->
            " perf: gen-us="
              <> show durationUs
              <> case tltGenerationBarsPerSecond t of
                   Nothing -> ""
                   Just barsPerSecond ->
                     " bars/s=" <> formatOneDecimal barsPerSecond
  in
    [ prefix
        <> " lookahead reason="
        <> tltReason t
        <> " generated-bars="
        <> show (tltGeneratedBarCount t)
        <> " range="
        <> show (tltFirstGeneratedBarIx t)
        <> "-"
        <> show (tltLastGeneratedBarIx t)
    , prefix
        <> "   frames="
        <> show (tltStartFrame t)
        <> "->"
        <> show (tltEndFrame t)
        <> " buffered-future-bars="
        <> show (tltBufferedFutureBars t)
        <> " targets: genre="
        <> tltGenre t
        <> " mood="
        <> showMaybeLabel (tltMoodTarget t)
        <> " energy="
        <> show (tltEnergyTarget t)
        <> " seed="
        <> show (tltSeedStart t)
        <> "->"
        <> show (tltSeedEnd t)
        <> perfSuffix
    ]

formatOneDecimal ∷ Float → String
formatOneDecimal value = showFFloat (Just 1) (realToFrac value ∷ Double) ""

renderTransitionTelemetryLines ∷ String → TimelineTransitionTelemetry → [String]
renderTransitionTelemetryLines prefix t =
  [ prefix
      <> " transition "
      <> ttFromSectionName t
      <> " -> "
      <> ttToSectionName t
  , prefix
      <> "   reason="
      <> ttReason t
      <> " boundary-bar="
      <> show (ttBoundaryBarIx t)
      <> " frame="
      <> show (ttBoundaryOffsetFrames t)
      <> " targets: mood="
      <> showMaybeLabel (ttMoodTarget t)
      <> " energy="
      <> show (ttEnergyTarget t)
      <> " pick="
      <> show (ttPickTicket t)
      <> "/"
      <> show (ttPickTotal t)
  , prefix
      <> "   base-weights: "
      <> formatSectionWeights (ttBaseWeights t)
  , prefix
      <> "   final-weights: "
      <> formatSectionWeights (ttFinalWeights t)
  ]

formatInstrumentCounts ∷ M.Map Int String → [(Int, Int)] → String
formatInstrumentCounts instrumentNameMap counts =
  if null counts
    then "none"
    else
      intercalate
        ", "
        [ M.findWithDefault ("instrument-" <> show iid) iid instrumentNameMap
            <> "#"
            <> show iid
            <> "×"
            <> show noteCount
        | (iid, noteCount) <- counts
        ]

formatSectionWeights ∷ [(String, Int)] → String
formatSectionWeights weights =
  if null weights
    then "none"
    else intercalate ", " [name <> "=" <> show weight | (name, weight) <- weights]

formatReasonCounts ∷ [(String, Int)] → String
formatReasonCounts reasons =
  if null reasons
    then "none"
    else intercalate ", " [reason <> "=" <> show count | (reason, count) <- reasons]

showMaybeLabel ∷ Maybe String → String
showMaybeLabel mValue =
  case mValue of
    Nothing -> "none"
    Just value -> value

dropTrailingBlank ∷ [String] → [String]
dropTrailingBlank xs =
  case reverse xs of
    "" : rest -> reverse rest
    _ -> xs

runInputMode ∷ AudioSystem → Runtime.PlayerSystem → FilePath → IO ()
runInputMode sys player inputPath
  | hasExt ".mid" inputPath || hasExt ".midi" inputPath = do
      playMidiInput sys inputPath
      putStrLn "Press q to quit."
      withRawStdin (waitForQuit player)
  | hasExt ".wav" inputPath = do
      playWavInput player inputPath
      putStrLn "Press q to quit."
      withRawStdin (waitForQuit player)
  | hasExt ".yaml" inputPath || hasExt ".yml" inputPath =
      playSongTimelineInteractive sys player inputPath
  | otherwise =
      ioError (userError ("Unsupported input file type: " <> inputPath <> "\n" <> usageText))

playMidiInput ∷ AudioSystem → FilePath → IO ()
playMidiInput sys inputPath = do
  planResult <- loadMidiPlaybackPlan inputPath
  case planResult of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right plan -> do
      _ <- forkIO $ do
        playbackResult <- playMidiPlaybackPlan defaultChannelMap sys plan
        case playbackResult of
          Left err -> hPutStrLn stderr err
          Right () -> pure ()
      pure ()
  putStrLn ("Playing MIDI file: " <> inputPath)

playWavInput ∷ Runtime.PlayerSystem → FilePath → IO ()
playWavInput player inputPath = do
  let clip = ClipId 0
  loadResult <- Runtime.loadClipFileChecked player clip inputPath
  case loadResult of
    Left err -> do
      hPutStrLn stderr ("Failed to load WAV clip: " <> err)
      exitFailure
    Right () -> pure ()
  Runtime.playMusicClipNow player clip 1 0 False
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
  | PromptStatus
  | PromptStart
  | PromptStop
  | PromptPanic
  | PromptPreviewNote !String !String !Int !Float !Int
  | PromptPreviewPatch !FilePath !Int !Float !Int
  | PromptUnknown !String

playSongTimelineInteractive ∷ AudioSystem → Runtime.PlayerSystem → FilePath → IO ()
playSongTimelineInteractive sys player path = do
  instCounterRef <- newIORef 1
  _ <- forkIO (playerEventPrinter player)
  loadResult <- Runtime.loadSongChecked player path
  case loadResult of
    Left err -> do
      hPutStrLn stderr ("Failed to load song timeline: " <> err)
      exitFailure
    Right () -> pure ()
  startResult <- Runtime.startSongNextBarChecked player
  case startResult of
    Left err -> do
      hPutStrLn stderr ("Failed to start song timeline: " <> err)
      exitFailure
    Right () -> pure ()
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

runNotePreviewShell ∷ AudioSystem → Runtime.PlayerSystem → IO ()
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

runPromptCommand ∷ AudioSystem → Runtime.PlayerSystem → IORef Word64 → PromptCmd → IO Bool
runPromptCommand sys player instCounterRef cmd =
  case cmd of
    PromptHelp -> printPromptHelp >> pure False
    PromptQuit -> do
      Runtime.stopSong player
      Runtime.panic player
      quitInteractive
      pure True
    PromptGenre genre -> do
      case genre of
        Nothing -> Runtime.clearGenreTarget player
        Just g -> Runtime.setGenreTarget player g
      pure False
    PromptMood mood -> do
      case mood of
        Nothing -> Runtime.clearMoodTarget player
        Just m -> Runtime.setMoodTarget player m
      pure False
    PromptEnergy x -> Runtime.setEnergyTarget player x >> pure False
    PromptAutoEnergy target bars curve -> do
      Runtime.automateEnergyNextBar player target bars curve
      pure False
    PromptAutoMood mood bars -> do
      let target = maybe "" id mood
      Runtime.automateMoodNextBar player target bars
      pure False
    PromptCancelEnergy ->
      Runtime.cancelEnergyAutomation player >> pure False
    PromptCancelMood ->
      Runtime.cancelMoodAutomation player >> pure False
    PromptTempo bpm ->
      Runtime.setTempoBpm player bpm >> pure False
    PromptMeter beats ->
      Runtime.setMeter player beats >> pure False
    PromptStatus ->
      Runtime.requestStatus player >> pure False
    PromptStart -> do
      startResult <- Runtime.startSongNextBarChecked player
      case startResult of
        Left err -> hPutStrLn stderr ("Failed to start song timeline: " <> err)
        Right () -> pure ()
      pure False
    PromptStop ->
      Runtime.stopSong player >> pure False
    PromptPanic ->
      Runtime.panic player >> pure False
    PromptPreviewNote genre instrumentName key velocity durationMs -> do
      previewGeneratedNote sys instCounterRef genre instrumentName key velocity durationMs
      pure False
    PromptPreviewPatch patchPath key velocity durationMs -> do
      previewPatchNote sys instCounterRef patchPath key velocity durationMs
      pure False
    PromptUnknown msg -> do
      putStrLn msg
      pure False

parsePromptCmd ∷ String → PromptCmd
parsePromptCmd raw =
  let ws = words raw
      lower = map (map toLower) ws
      tabs = splitTabs raw
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
    case tabs of
      ["patch", patchPath, keyS] ->
        case parseInt keyS of
          Just key -> PromptPreviewPatch patchPath key 0.72 360
          Nothing -> PromptUnknown "Usage: patch <path> <midi-key> [velocity 0..1] [duration-ms]"
      ["patch", patchPath, keyS, velocityS] ->
        case (parseInt keyS, parseFloat velocityS) of
          (Just key, Just velocity) -> PromptPreviewPatch patchPath key velocity 360
          _ -> PromptUnknown "Usage: patch <path> <midi-key> [velocity 0..1] [duration-ms]"
      ["patch", patchPath, keyS, velocityS, durationS] ->
        case (parseInt keyS, parseFloat velocityS, parseInt durationS) of
          (Just key, Just velocity, Just durationMs) -> PromptPreviewPatch patchPath key velocity durationMs
          _ -> PromptUnknown "Usage: patch <path> <midi-key> [velocity 0..1] [duration-ms]"
      _ ->
        case lower of
          [] -> PromptHelp
          ["help"] -> PromptHelp
          ["h"] -> PromptHelp
          ["q"] -> PromptQuit
          ["quit"] -> PromptQuit
          ["exit"] -> PromptQuit
          ["panic"] -> PromptPanic
          ["status"] -> PromptStatus
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
  where
    splitTabs line =
      case map trim (splitOnTabs line) of
        [] -> []
        parts -> filter (not . null) parts

    splitOnTabs line =
      case break (== '\t') line of
        (chunk, []) -> [chunk]
        (chunk, _ : rest) -> chunk : splitOnTabs rest

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
      , "  status                   Query current runtime status snapshot"
      , "  note <genre> <instrument> <midi-key> [velocity] [duration-ms]"
      , "                           Preview one generated instrument note"
      , "  patch <path> <midi-key> [velocity] [duration-ms]"
      , "                           Preview one external patch file"
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

playerEventPrinter ∷ Runtime.PlayerSystem → IO ()
playerEventPrinter player = loop
  where
    loop = do
      m <- Runtime.tryReadEvent player
      case m of
        Nothing -> threadDelay 20000 >> loop
        Just ev -> do
          printPlayerEvent ev
          loop

printPlayerEvent ∷ Runtime.PlayerEvent → IO ()
printPlayerEvent ev =
  case ev of
    Runtime.PlayerEventStatus status ->
      putStrLn
        ( "[player] status frame="
            <> show (Runtime.prsTransportFrame status)
            <> " loaded="
            <> show (Runtime.prsSongLoaded status)
            <> " playing="
            <> show (Runtime.prsTimelinePlaying status)
            <> " section="
            <> show (Runtime.prsCurrentSection status)
        )
    Runtime.PlayerEventTimelineLoaded path ->
      putStrLn ("[player] loaded timeline: " <> path)
    Runtime.PlayerEventTimelineLoadFailed path err ->
      putStrLn ("[player] timeline load failed (" <> path <> "): " <> err)
    Runtime.PlayerEventClipLoaded clipId path ->
      putStrLn ("[player] loaded clip " <> show clipId <> ": " <> path)
    Runtime.PlayerEventClipLoadFailed clipId path err ->
      putStrLn ("[player] clip load failed (" <> show clipId <> ", " <> path <> "): " <> err)
    Runtime.PlayerEventTimelineStarted frame bars ->
      putStrLn ("[player] timeline started at frame " <> show frame <> " (" <> show bars <> " bars)")
    Runtime.PlayerEventTimelineStopped ->
      putStrLn "[player] timeline stopped"
    Runtime.PlayerEventTimelineFinished ->
      putStrLn "[player] timeline finished"
    Runtime.PlayerEventTimelineRetargeted t ->
      mapM_ putStrLn (renderRetargetTelemetryLines "[player]" t)
    Runtime.PlayerEventTimelineLookahead t ->
      mapM_ putStrLn (renderLookaheadTelemetryLines "[player]" t)
    Runtime.PlayerEventTimelineTransition t ->
      mapM_ putStrLn (renderTransitionTelemetryLines "[player]" t)
    Runtime.PlayerEventEnergyAutomationStarted startF endF fromE toE curve ->
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
    Runtime.PlayerEventEnergyAutomationCompleted endF finalE ->
      putStrLn ("[player] energy lane completed frame=" <> show endF <> " value=" <> show finalE)
    Runtime.PlayerEventMoodAutomationStarted startF endF fromM toM ->
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
    Runtime.PlayerEventMoodAutomationCompleted endF finalM ->
      putStrLn ("[player] mood lane completed frame=" <> show endF <> " value=" <> show finalM)
    Runtime.PlayerEventEnergyAutomationCanceled ->
      putStrLn "[player] energy lane canceled"
    Runtime.PlayerEventMoodAutomationCanceled ->
      putStrLn "[player] mood lane canceled"
    Runtime.PlayerEventGenreChanged genre ->
      putStrLn ("[player] genre set to " <> genre)
    Runtime.PlayerEventGenreChangeRejected requested err ->
      putStrLn ("[player] genre change rejected (" <> requested <> "): " <> err)
    Runtime.PlayerEventScheduled _ _ ->
      pure ()
    Runtime.PlayerEventAudio _ ->
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
          useBuiltInDrums = usesBuiltInDrumPatch instrumentSpec
          previewIid =
            if useBuiltInDrums
              then previewDrumInstrumentId normalizedKey
              else baseIid
          previewKey =
            if useBuiltInDrums
              then NoteKey 60
              else NoteKey normalizedKey
      noteInst <- nextPreviewInstanceId instCounterRef
      loadResult <- loadPreviewInstrument instrumentSpec normalizedKey previewIid
      case loadResult of
        Left err ->
          putStrLn ("[preview] " <> err)
        Right () -> do
          sendAudio
            sys
            (AudioNoteOn
              { instrumentId = previewIid
              , noteBus = AudioBusSfx
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
  where
    loadPreviewInstrument instrumentSpec normalizedKey previewIid =
      if usesBuiltInDrumPatch instrumentSpec
        then do
          sendAudio sys (AudioLoadInstrument previewIid (gmDrumInstrument normalizedKey))
          pure (Right ())
        else do
          instResult <- loadResolvedInstrumentEither instrumentSpec
          case instResult of
            Left err ->
              pure (Left err)
            Right inst -> do
              sendAudio sys (AudioLoadInstrument previewIid inst)
              pure (Right ())

nextPreviewInstanceId ∷ IORef Word64 → IO NoteInstanceId
nextPreviewInstanceId ref =
  NoteInstanceId <$> atomicModifyIORef' ref (\n -> (n + 1, n))

previewDrumInstrumentId ∷ Int → InstrumentId
previewDrumInstrumentId key =
  InstrumentId (128 + max 0 (min 127 key))

previewPatchNote ∷ AudioSystem → IORef Word64 → FilePath → Int → Float → Int → IO ()
previewPatchNote sys instCounterRef patchPath key velocity durationMs = do
  let normalizedKey = max 0 (min 127 key)
      velocity' = max 0 (min 1 velocity)
      durationUs = max 1 durationMs * 1000
      previewIid = InstrumentId 255
      previewKey = NoteKey normalizedKey
  noteInst <- nextPreviewInstanceId instCounterRef
  loadResult <- loadInstrumentPatchEither patchPath
  case loadResult of
    Left err ->
      putStrLn ("[preview] " <> err)
    Right inst -> do
      sendAudio sys (AudioLoadInstrument previewIid inst)
      sendAudio
        sys
        (AudioNoteOn
          { instrumentId = previewIid
          , noteBus = AudioBusSfx
          , amp = 1
          , pan = 0
          , noteKey = previewKey
          , noteInstanceId = noteInst
          , velocity = velocity'
          , adsrOverride = Nothing
          })
      _ <- forkIO $ do
        threadDelay durationUs
        sendAudio sys (AudioNoteOff previewIid noteInst)
      putStrLn
        ( "[preview] patch "
            <> patchPath
            <> " note "
            <> show normalizedKey
            <> " velocity="
            <> show velocity'
            <> " durationMs="
            <> show (max 1 durationMs)
        )

waitForQuit ∷ Runtime.PlayerSystem → IO ()
waitForQuit player = do
  let loop = do
        c <- hGetChar stdin
        case c of
          'q' -> do
            Runtime.panic player
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
