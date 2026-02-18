{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.IORef
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO

import Audio.Thread
import Audio.Types
import Audio.Envelope
import Audio.Filter.Types

import Midi.Play (playMidiFile, defaultChannelMap)

main ∷ IO ()
main = do
  args <- getArgs
  case args of
    [midiPath] ->
      bracket startAudioSystem stopAudioSystem $ \sys -> do
        -- Preload instruments 0..15 (MIDI channels) with the same patch for now.
        preloadChannels sys

        -- Start MIDI playback in a separate thread.
        _ <- forkIO $ playMidiFile defaultChannelMap sys midiPath

        putStrLn $ "Playing MIDI file: " <> midiPath
        putStrLn "Press q to quit."
        withRawStdin $ waitForQuit sys

    _ -> do
      putStrLn "usage: your-exe <file.mid>"
      exitSuccess

--------------------------------------------------------------------------------

preloadChannels ∷ AudioSystem → IO ()
preloadChannels sys = do
  let envAmpNormal = ADSR 0.02 0.10 0.6 0.50

      basePitch = PitchSpec 0 0 0 0
      mkRoutes enabled =
        if not enabled
          then []
          else
            [ ModRoute ModSrcLfo1 ModDstFilterCutoffOct 0.5
            , ModRoute ModSrcLfo1 ModDstAmpGain (-0.3)
            , ModRoute ModSrcEnvAmp (ModDstLayerPitchCents 0) (-8)
            ]

      mkInst ∷ Instrument
      mkInst =
        Instrument
          { iOscs =
              [ OscLayer WaveSaw basePitch 1.0 NoSync
              ]
          , iLayerSpread = 0.7
          , iAdsrDefault = envAmpNormal
          , iGain = 1.0
          , iFilter = Nothing
          , iModRoutes = mkRoutes True
          , iPlayMode = Poly
          , iPolyMax = 8
          , iVoiceSteal = StealQuietest
          }

  forM_ [0..15] $ \ch ->
    sendAudio sys (AudioSetInstrument (InstrumentId ch) mkInst)

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
