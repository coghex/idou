# Idou

Haskell FFI to miniaudio for playback.

Runtime audio settings live in `config/audio.yaml` under the `audio:` key.
Timeline song settings can live in `config/song.yaml` and be loaded by the `Player.Thread` timeline API (`PlayerLoadSongTimeline` + `PlayerStartSongTimeline`).

MIDI playback currently supports note on/off, tempo changes, General MIDI-style Program Change patch switching, channel pressure aftertouch, polyphonic key pressure aftertouch, pitch bend (default range ±2 semitones), and controller messages for mod wheel (CC1), channel volume (CC7), pan (CC10), expression (CC11), sustain pedal (CC64), all sound off (CC120), reset all controllers (CC121), and all notes off (CC123). Channel 10 follows the GM percussion convention and now maps common drum keys to a compact synthesized kit with family fallbacks. The drum engine also has a noise source that can produce white noise, pink noise, or a blend of the two, with per-layer envelopes for snare and cymbal-style transients.

Run with:

```bash
cabal run idou -- path/to/file.mid
cabal run idou -- path/to/file.wav
cabal run idou -- config/song.yaml
```

When launching with `.yaml`/`.yml`, the CLI opens an interactive prompt (`idou>`) for live control (`help` shows commands like `mood`, `energy`, `auto-energy`, `auto-mood`, `tempo`, `meter`, `start`, `stop`, `panic`, `quit`).

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
- `instruments.<name>` with `instrument_id`, optional `amp`, `pan`, optional `velocity_variation` (0..1), and section patterns via either:
  - `patterns.<section>: ...`
  - `pattern_<section>: ...`
  - optional drum fill patterns per section via either:
    - `fills.<section>: ...`
    - `fill_<section>: ...`

At runtime, `Player.Thread` now runs a conductor that advances sections at phrase boundaries using Haskell-defined weighted transitions (not YAML-defined transition graphs).
Cue-mode transition rules are structured for song form: `intro → verse`, then `verse/chorus/bridge → chorus|bridge|ending(outro)`, and timelines always resolve to an ending/outro section.
The runtime also applies deterministic per-bar variation (density gating, transposition windows, and instrument-layer dropout) so repeated section patterns evolve without changing YAML.
Variation is section-aware: choruses are biased to remain stable while bridges are biased to mutate more.
Live control is available via player messages: `PlayerSetMoodTarget`, `PlayerClearMoodTarget`, and `PlayerSetEnergyTarget`; these steer both section selection and variation intensity while the timeline is running.
Runtime automation lanes are available via `PlayerAutomateEnergyNextBar` (with `AutomationStep|AutomationLinear|AutomationEaseInOut`), `PlayerAutomateMoodNextBar`, and lane cancellation messages; energy ramps are evaluated each player tick and applied back into timeline targets.
Transition observability is exposed via `PlayerEventTimelineTransition`, which reports section boundary transitions with from/to sections, reason, candidate weights (base + final), boundary timing, active mood/energy targets, and weighted-pick ticket info.

If a section has no explicit drum pattern, timeline generation now injects a fallback drum groove on instrument/channel 10 (`InstrumentId 9`) so tracks keep percussion by default.
For drum instruments (channel/instrument 10, `InstrumentId 9`), fills are applied automatically on phrase-boundary bars when a section fill is defined.
Timeline drum notes now load per-key GM drum patches at scheduling time, so kick/snare/hat/tom keys produce distinct drum timbres during timeline playback.

Pattern note syntax is a comma-separated list of `beat/key/duration/velocity`, for example:

```yaml
instruments:
  bass:
    instrument_id: 1
    patterns:
      verse: 0/40/0.5/0.8,2/43/0.5/0.8
```

When `velocity_variation > 0`, note velocity is humanized deterministically: downbeats are accented (beat 1 strongest, beat 3 secondary in 4/4-style bars) plus light per-hit jitter; `0` disables this and preserves YAML velocities exactly.
