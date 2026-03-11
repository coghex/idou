# Idou

Haskell FFI to miniaudio for playback.

Runtime audio settings live in `config/audio.yaml` under the `audio:` key.
Timeline song settings can live in `config/song.yaml` and be loaded by the `Player.Thread` timeline API (`PlayerLoadSongTimeline` + `PlayerStartSongTimeline`).

MIDI playback currently supports note on/off, tempo changes, General MIDI-style Program Change patch switching, channel pressure aftertouch, polyphonic key pressure aftertouch, pitch bend (default range ±2 semitones), and controller messages for mod wheel (CC1), channel volume (CC7), pan (CC10), expression (CC11), sustain pedal (CC64), all sound off (CC120), reset all controllers (CC121), and all notes off (CC123). Channel 10 follows the GM percussion convention and now maps common drum keys to a compact synthesized kit with family fallbacks. The drum engine also has a noise source that can produce white noise, pink noise, or a blend of the two, with per-layer envelopes for snare and cymbal-style transients.

Run with:

```bash
cabal run idou -- path/to/file.mid
cabal run idou -- path/to/file.wav
cabal run idou -- song1.yaml
cabal run idou -- config/song.yaml
```

## Idou Studio

The repository now also includes a companion desktop editor in `studio/` for authoring the high-level song-intent YAML format visually.

Current studio workflow:
- song-level editing for `genre`, `mood`, `tempo_bpm`, `beats_per_bar`, and `beat_unit`
- section/form editing with reorderable sections
- chord editing per section
- canvas-based melody grid for note entry, drag/resize editing, and selection
- DAW-style transport-first layout with snap and zoom controls for the piano roll
- looped selected-section playback plus play/pause transport control (`Space` toggles play/pause)
- common editor shortcuts for save/open, note duplication, deletion, and keyboard nudging
- generated accompaniment visualization overlaid on the melody grid, using the Haskell arranger output
- generated YAML preview
- open/save dialogs for `.yaml` / `.yml`
- preview playback by staging YAML and launching the existing headless `idou` engine

To run the studio in development:

```bash
cd studio
npm install
npm run tauri dev
```

The studio is intentionally a separate tool: the Haskell engine remains the playback source of truth, and the GUI shells out to `cabal run idou -- <staged-preview.yaml>` for preview.

When launching with `.yaml`/`.yml`, the CLI opens an interactive prompt (`idou>`) for live control (`help` shows commands like `genre`, `mood`, `energy`, `auto-energy`, `auto-mood`, `tempo`, `meter`, `start`, `stop`, `panic`, `quit`).

Optional runtime health telemetry:

```bash
cabal run idou -- --audio-health=verbose path/to/file.mid
```

Telemetry behavior can be tuned in `config/audio.yaml` under `audio.telemetry`:
- `verbose_report_every_loops`
- `partial_write_alert_threshold`

## Timeline song YAML

The recommended schema is now a higher-level song-intent format:
- `song.genre` selects an arranger pack (currently `electronic`, `ambient`, `blackmetal`, and `cinematic`)
- `song.mood` provides the base mood target for arrangement and conductor weighting
- `song.tempo_bpm`, `song.beats_per_bar`, optional `song.beat_unit` provide section defaults
- `song.form` is a comma-separated section order such as `intro,verse,chorus,bridge,outro`
- `sections.<name>` defines `bars_per_phrase`, `phrase_count`, optional `mood`, `feel`, plus:
  - `chords: Am,F,C,G` for one chord per bar in the phrase
  - `melody: 0.0/A4/0.5/0.62,1.5/C5/0.5/0.58,...` where beat offsets are phrase-relative

Example:

```yaml
song:
  genre: electronic
  mood: dramatic
  tempo_bpm: 100
  beats_per_bar: 4
  form: intro,verse,chorus,outro

sections:
  intro:
    bars_per_phrase: 4
    phrase_count: 1
    chords: Am(add9),Fmaj7,Dm7,Esus4
    melody: 2.0/A4/1.0/0.54,3.0/C5/0.75/0.58,3.75/E5/0.25/0.62
```

For the new schema, the arranger generates drums, bass, pad, arp, and lead layers in Haskell from genre + mood + harmony/melody, and drum fills still honor the existing generated-fill system. Basslines and accompaniment now use chord-aware support tones, melody-guided extension tones, approach notes into upcoming roots, and smoother pad voicings so the generated harmony reacts to both the progression and the authored melodic anchor instead of repeating a fixed root/fifth loop. While the timeline is playing, `genre <name|default>` swaps the active arranger pack for newly generated bars, so you can move between supported genres live.

The older deterministic per-instrument schema is still supported:
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
