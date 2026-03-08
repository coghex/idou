# Copilot Instructions

## Build, test, and lint

- Build: `cabal build`
- Run locally: `cabal run idou -- path/to/file.mid`
- Run all tests: `cabal test idou-tests --test-show-details=direct`
- Run a single test (name filter): `cabal test idou-tests --test-show-details=direct --test-options="mono-legato-reuses-voice-and-updates-override"`
- Lint/format: no repository-specific lint or formatter command is configured in the repo.
- Runtime audio tuning lives in `config/audio.yaml`; the executable expects that file to exist and uses the nested `audio:` YAML shape.

## High-level architecture

- `app/Main.hs` is a thin CLI entrypoint. It expects one MIDI path, starts the audio system with `bracket`, preloads one synth patch onto MIDI channels `0..15`, forks `Midi.Play.playMidiFile`, and waits for raw-stdin `q` to stop playback.
- `src/Midi/Play.hs` converts parsed MIDI data into timed `AudioMsg` traffic. It uses the embedded `src/Midi/ZMidi/Core/*` parser, flattens tracks into absolute-tick events, tracks held notes by `(channel, key)`, and maps MIDI channels to `InstrumentId` values through `MidiChannelMap`.
- `src/Audio/Thread.hs` owns the control/audio boundary. `sendAudio` enqueues `AudioMsg` values onto the STM queue, `runAudioLoop` drains messages and mutates `AudioState`, and `renderIfNeeded` keeps the ring buffer ahead of the miniaudio callback.
- `src/Audio/Thread/Voice.hs`, `src/Audio/Thread/Render.hs`, and `src/Audio/Thread/InstrumentTable.hs` split the engine responsibilities: voice allocation/release and voice stealing, per-sample rendering/modulation/filtering, and per-channel instrument/glide/legato/vibrato tables.
- `src/Sound/Miniaudio.hs` and `src/Sound/Miniaudio/RingBuffer.hs` are the FFI edge to the C code in `cbits/miniaudio*.c`. The callback pulls frames from the ring buffer and zero-fills underruns.
- `test/Main.hs` is a lightweight regression harness for deterministic behavior. It avoids live device startup and instead checks pure or in-memory logic such as envelope stepping and voice-management regressions.

## Key conventions

- Most modules use `{-# LANGUAGE Strict, UnicodeSyntax #-}`. Match the existing style by keeping Unicode syntax such as `∷` and `→` in Haskell source.
- Record fields are prefixed by their owning type (`st*`, `v*`, `i*`, `ol*`, `ps*`, `mr*`, `ts*`, `as*`). Reuse those prefixes instead of introducing new naming schemes.
- Cross-thread communication goes through `AudioMsg` and `Engine.Core.Queue`; avoid mutating audio-engine state from the CLI or MIDI side. The real-time engine state lives in `IORef AudioState` plus `Data.Vector.Mutable` tables for voices and per-instrument settings.
- Live voices are tracked in a compact mutable vector. `Render.mixVoices` removes finished voices by swapping the last active voice into the current slot, so iterate by `stActiveCount`, not by the full vector length.
- Voice identity is MIDI-native: `InstrumentId` usually follows the MIDI channel, while note lifetime is tracked with both `NoteKey` and `NoteInstanceId`. `Midi.Play` depends on that pairing to handle repeated `NoteOn`/`NoteOff` events correctly.
- Initialization and FFI setup fail fast with `error` on null pointers or failing miniaudio calls; keep failures explicit instead of adding silent recovery paths in low-level audio code.
- Shared audio constants live in `Audio.Thread.Types` (`sr`, `maxLayers`). Keep those aligned with changes to rendering or device setup.
