{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO

import Audio.Config (audioConfigPath, loadAudioConfig)
import Audio.Patch (defaultMidiProgram, gmChannelInstrument)
import Audio.Thread
import Audio.Types

import Midi.Play (playMidiFile, defaultChannelMap)

main ∷ IO ()
main = do
  args <- getArgs
  case parseCliArgs args of
    Right (healthVerbosity, midiPath) -> do
      audioCfg <- loadAudioConfig audioConfigPath
      bracket (startAudioSystem audioCfg healthVerbosity) stopAudioSystem $ \sys -> do
        -- Preload instruments 0..15 (MIDI channels) with the same patch for now.
        preloadChannels sys

        -- Start MIDI playback in a separate thread.
        _ <- forkIO $ playMidiFile defaultChannelMap sys midiPath

        putStrLn $ "Playing MIDI file: " <> midiPath
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
    [ "usage: idou [--audio-health|--audio-health=off|--audio-health=normal|--audio-health=verbose] <file.mid>"
    , "  --audio-health defaults to normal and reports runtime health on anomalies."
    ]

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
            -- release everything on quit
            forM_ [0..15] $ \ch ->
              sendAudio sys (AudioNoteOffInstrument (InstrumentId ch))
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
