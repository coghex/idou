{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (isSuffixOf, stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO

import Audio.Config (audioConfigPath, loadAudioConfig)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument)
import Audio.Thread
import Audio.Types
import Player.Thread

import Midi.Play (playMidiFile, defaultChannelMap)

main ∷ IO ()
main = do
  args <- getArgs
  case parseCliArgs args of
    Right (healthVerbosity, inputPath) -> do
      audioCfg <- loadAudioConfig audioConfigPath
      bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
        bracket (startPlayerThread sys) stopPlayerThread $ \_player -> do
          -- Preload instruments 0..15 (MIDI channels) with the same patch for now.
          preloadChannels sys

          playInput sys inputPath
          putStrLn "Press q to quit."
          withRawStdin $ waitForQuit sys

    Left usage -> do
      putStrLn usage
      exitSuccess

--------------------------------------------------------------------------------

parseCliArgs ∷ [String] → Either String (AudioHealthVerbosity, FilePath)
parseCliArgs = go AudioHealthNormal Nothing
  where
    go verbosity midiPath [] =
      case midiPath of
        Just p -> Right (verbosity, p)
        Nothing -> Left usageText
    go verbosity midiPath (arg:rest) =
      case arg of
        "--audio-health" ->
          go AudioHealthNormal midiPath rest
        "--audio-health=off" ->
          go AudioHealthOff midiPath rest
        "--audio-health=normal" ->
          go AudioHealthNormal midiPath rest
        "--audio-health=verbose" ->
          go AudioHealthVerbose midiPath rest
        _ ->
          case stripPrefix "--audio-health=" arg of
            Just _ ->
              Left ("Unknown audio health verbosity in " <> arg <> "\n" <> usageText)
            Nothing ->
              if take 2 arg == "--"
                then Left ("Unknown option " <> arg <> "\n" <> usageText)
                else
                  case midiPath of
                    Nothing -> go verbosity (Just arg) rest
                    Just _ -> Left usageText

usageText ∷ String
usageText =
  unlines
    [ "usage: idou [--audio-health|--audio-health=off|--audio-health=normal|--audio-health=verbose] <file.mid|file.midi|file.wav>"
    , "  --audio-health defaults to normal and reports runtime health on anomalies."
    ]

playInput ∷ AudioSystem → FilePath → IO ()
playInput sys inputPath
  | hasExt ".mid" inputPath || hasExt ".midi" inputPath = do
      _ <- forkIO $ playMidiFile defaultChannelMap sys inputPath
      putStrLn $ "Playing MIDI file: " <> inputPath
  | hasExt ".wav" inputPath = do
      let clip = ClipId 0
      loadClipFromFile sys clip inputPath
      sendAudio sys (AudioPlayClip clip AudioBusMusic 1 0 False)
      putStrLn $ "Playing WAV file: " <> inputPath
  | otherwise =
      ioError (userError ("Unsupported input file type: " <> inputPath <> "\n" <> usageText))

hasExt ∷ String → FilePath → Bool
hasExt ext path = ext `isSuffixOf` map toLower path

preloadChannels ∷ AudioSystem → IO ()
preloadChannels sys = do
  forM_ [0..15] $ \ch ->
    sendAudio
      sys
      (AudioLoadInstrument (InstrumentId ch) (gmChannelInstrument ch defaultMidiProgram))

  -- Default vibrato/LFO off.
  forM_ [0..15] $ \ch ->
    sendAudio sys (AudioSetVibrato (InstrumentId ch) 0 0)

waitForQuit ∷ AudioSystem → IO ()
waitForQuit sys = do
  -- If you want a "panic" key too, add it here.
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
  oldBuf  <- hGetBuffering stdin
  bracket
    (do hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        pure (oldEcho, oldBuf))
    (\(e,b) -> do
        hSetEcho stdin e
        hSetBuffering stdin b)
    (\_ -> action)
