# Idou

Haskell FFI to miniaudio for playback.

Runtime audio settings live in `config/audio.yaml` under the `audio:` key.
Timeline song settings can live in `config/song.yaml` and be loaded by the `Player.Thread` timeline API (`PlayerLoadSongTimeline` + `PlayerStartSongTimeline`).

MIDI playback currently supports note on/off, tempo changes, General MIDI-style Program Change patch switching, channel pressure aftertouch, polyphonic key pressure aftertouch, pitch bend (default range ±2 semitones), and controller messages for mod wheel (CC1), channel volume (CC7), pan (CC10), expression (CC11), sustain pedal (CC64), all sound off (CC120), reset all controllers (CC121), and all notes off (CC123). Channel 10 follows the GM percussion convention and now maps common drum keys to a compact synthesized kit with family fallbacks. The drum engine also has a noise source that can produce white noise, pink noise, or a blend of the two, with per-layer envelopes for snare and cymbal-style transients.

Run with:

```bash
cabal run idou -- path/to/file.mid
cabal run idou -- path/to/file.wav
```

Optional runtime health telemetry:

```bash
cabal run idou -- --audio-health=verbose path/to/file.mid
```

Telemetry behavior can be tuned in `config/audio.yaml` under `audio.telemetry`:
- `verbose_report_every_loops`
- `partial_write_alert_threshold`

## Timeline song YAML (deterministic v1)

The timeline parser supports:
- `song.mode: cue|drone`
- `song.lookahead_bars`
- `sections.<name>` with `tempo_bpm`, `beats_per_bar`, optional `beat_unit`, `bars_per_phrase`, `phrase_count`, `mood`, `feel`
- `instruments.<name>` with `instrument_id`, optional `amp`, `pan`, and section patterns via either:
  - `patterns.<section>: ...`
  - `pattern_<section>: ...`

At runtime, `Player.Thread` now runs a conductor that advances sections at phrase boundaries using Haskell-defined weighted transitions (not YAML-defined transition graphs).

Pattern note syntax is a comma-separated list of `beat/key/duration/velocity`, for example:

```yaml
instruments:
  bass:
    instrument_id: 1
    patterns:
      verse: 0/40/0.5/0.8,2/43/0.5/0.8
```
