# Idou

Idou is an engine-first adaptive music runtime for a Haskell game, with a separate Studio app for authoring the song YAML and patch data that the runtime consumes.

Runtime audio settings live in `config/audio.yaml` under the `audio:` key.
Timeline song settings can live in `config/song.yaml` and be loaded by the `Player.Thread` timeline API (`PlayerLoadSongTimeline` + `PlayerStartSongTimeline`).

MIDI playback currently supports note on/off, tempo changes, General MIDI-style Program Change patch switching, channel pressure aftertouch, polyphonic key pressure aftertouch, pitch bend (default range ±2 semitones), and controller messages for mod wheel (CC1), channel volume (CC7), pan (CC10), expression (CC11), sustain pedal (CC64), all sound off (CC120), reset all controllers (CC121), and all notes off (CC123). Channel 10 follows the GM percussion convention and now maps common drum keys to a compact synthesized kit with family fallbacks. The drum engine also has a noise source that can produce white noise, pink noise, or a blend of the two, with per-layer envelopes for snare and cymbal-style transients.

Run with:

```bash
cabal run idou -- path/to/file.mid
cabal run idou -- path/to/file.wav
cabal run idou -- song1.yaml
cabal run idou -- config/song.yaml
cabal run idou -- --explain-timeline config/song.yaml
cabal run idou -- --audio-health=verbose config/song.yaml
cabal run idou -- --baseline-report --baseline-bars 16 config/song.yaml
cabal run idou -- --baseline-report --baseline-scenario rapid-retarget config/song.yaml
```

## Game runtime contract

The intended game-facing control surface now lives in `src/Player/Runtime.hs`.

Use that module as the façade for gameplay integration. It wraps the lower-level `Player.Thread` and `Audio.Thread` messages into a smaller runtime API aimed at:

- starting/stopping the runtime thread and reading runtime events
- loading and starting adaptive song timelines
- stopping, pausing, and resuming playback
- loading clip assets for later playback
- changing mood and energy targets while a song is running
- querying runtime status snapshots (transport frame, current section, loaded/playing flags)
- scheduling music/SFX clips on the shared transport
- triggering immediate clip and note playback through the player thread, with explicit bus ownership when needed
- issuing a full panic/stop-all command
- optionally reading the shared transport frame directly from the audio system when needed

### Runtime roles

- `Audio.Thread` owns the mixer, transport frame, clip playback, buses, and direct note/voice control.
- `Player.Thread` owns adaptive song state, lookahead generation, timeline scheduling, transport-aligned adaptive control, clip-load proxying, immediate one-shot dispatch, and status snapshot replies.
- `Player.Runtime` is the preferred game-facing façade over those lower-level modules; `app/Main.hs` now uses it instead of raw `PlayerMsg` constructors for runtime control.
- `app/Main.hs` and Studio are runtime clients; they should not be treated as the source of truth for playback behavior.

### Supported mixed playback

The runtime is expected to mix these source types together in one engine:

- adaptive song timelines
- WAV clips
- MIDI file playback
- direct note playback / one-shot musical events

The current bus model is:

- `AudioBusMusic`
- `AudioBusSfx`

And the current source routing contract is:

- adaptive timeline notes route to `AudioBusMusic`
- MIDI playback routes note events to `AudioBusMusic`
- clips can be placed on either bus
- direct runtime note playback can be targeted explicitly by bus; the convenience `Player.Runtime.noteOnNow` / `scheduleNoteOnNextBar` helpers default to `AudioBusSfx`
- bus gain and bus stop now apply consistently to both clip sources and synthesized note voices

This is intentionally still a minimal mixer model. There are no sub-buses, ducking rules, source priorities, or separate voice budgets per bus yet; those remain future design work if game integration proves they are needed.

### Timing semantics

The runtime contract currently uses three timing buckets:

- `RuntimeImmediate`: applied as soon as the player or audio thread receives the message
- `RuntimeNextBar`: aligned to the next transport bar boundary
- `RuntimeFutureLookahead`: affects only future adaptive generation that has not already been scheduled

In practical terms:

- `loadSong`, `stopSong`, `pausePlayback`, `resumePlayback`, `setMoodTarget`, `clearMoodTarget`, `setEnergyTarget`, direct clip playback, direct note playback, and `panic` are immediate controls.
- `startSongNextBar`, automation lanes, and `schedule*NextBar` helpers are bar-synchronized controls.
- live genre switching remains a lower-level `Player.Thread` capability intended mainly for CLI/Studio/debug flows; it is not part of the primary game-facing runtime façade.

### Runtime status and events

- `Player.Runtime.requestStatus` asks the player thread for a `PlayerEventStatus` snapshot.
- `PlayerEventStatus` reports the current transport frame, whether a song is loaded, whether a timeline is actively playing, the current section when applicable, tempo/meter, mood/energy targets, active genre, and the count of buffered future bars.
- Clip asset load success/failure is reported via `PlayerEventClipLoaded` / `PlayerEventClipLoadFailed`.
- Existing timeline, automation, scheduled-action, clip-finished, and note-finished events remain available through the same event stream.

### Transport guarantees

- The audio thread owns the transport frame counter (`Audio.Thread.readTransportFrame`).
- Transport-aligned scheduling is done in absolute frame space.
- `Player.Thread` starts timelines on the next bar boundary using `nextBarFrame`.
- When a timeline is already active, next-bar scheduling consults the timeline runtime first so new work stays aligned with the adaptive song's current bar structure.
- Mood, energy, and live genre changes update adaptive targets immediately, but the runtime treats already-buffered lookahead bars as stable. The new targets apply from the next bar that has not been generated yet.
- That “future-generated-bars-only” policy is now surfaced through `TimelineRetargetTelemetry` / `PlayerEventTimelineRetargeted`, including how many future bars were already buffered and the frame where newly generated material will begin.
- Lookahead extension is also observable through `TimelineLookaheadTelemetry` / `PlayerEventTimelineLookahead`, which reports why bars were generated (`lookahead-horizon`, next-bar query, or boundary-span query), how many bars were added, the generated frame range, and the seed range used for deterministic replay.
- Live player-thread lookahead telemetry now also includes measured generation cost when available (`gen-us` and derived `bars/s`), so adaptive-arrangement regressions are visible during normal playback.
- For repeatable performance checks, `cabal run idou -- --baseline-report [--baseline-scenario <name>] [--baseline-bars <count>] <song.yaml>` runs the live engine for a fixed number of deterministic bars and prints a compact summary of aggregated lookahead and audio metrics.
- Built-in baseline scenarios currently include `steady-song`, `rapid-retarget`, `genre-sweep`, and `long-lookahead`; if you omit `--baseline-bars`, the runner uses the selected scenario's default length.
- For CLI debugging, `cabal run idou -- --explain-timeline <song.yaml>` now drains the timeline runtime offline and prints a readable explain trace: lookahead generation, weighted section transitions, retarget state, and per-bar instrument/note summaries.

## Idou Studio

The repository also includes a companion desktop editor in `studio/` for authoring the high-level song-intent YAML format visually.

Current studio workflow:
- song-level editing for `genre`, `mood`, `tempo_bpm`, `beats_per_bar`, and `beat_unit`
- generated-role patch assignment via `song.patches.*`, with patch library browsing and portable relative-path saves where possible
- section/form editing with reorderable sections
- chord editing per section, with compact presets, roman-numeral feedback, and transpose helpers
- canvas-based melody grid for note entry, drag/resize editing, and selection
- DAW-style transport-first layout with snap and zoom controls for the piano roll
- piano-key auditioning for generated `lead`, `pad`, `arp`, and `bass` layers using the current song genre
- patch editing for the modular YAML format: node/connection browser, graph-canvas patcher with drag-to-wire cables, inline node controls, inline wire editing, inspector controls, YAML preview, runtime validation, and direct patch audition
- looped selected-section playback plus play/pause transport control (`Space` toggles play/pause)
- common editor shortcuts for save/open, undo/redo, note clipboard, duplication, deletion, and keyboard nudging
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
Patch validation uses `cabal run idou -- --check-patch <patch.yaml>`, and patch auditioning uses the note-preview shell with direct external patch-file preview commands.

When launching with `.yaml`/`.yml`, the CLI opens an interactive prompt (`idou>`) for live control (`help` shows commands like `genre`, `mood`, `energy`, `auto-energy`, `auto-mood`, `tempo`, `meter`, `start`, `stop`, `panic`, `quit`).

Optional runtime health telemetry:

```bash
cabal run idou -- --audio-health=verbose path/to/file.mid
```

Telemetry behavior can be tuned in `config/audio.yaml` under `audio.telemetry`:
- `verbose_report_every_loops`
- `partial_write_alert_threshold`

## Experimental patch graph YAML

The repository now includes an experimental voice-local patch loader/compiler in `Audio.Patch.Loader`.

- Patch files live separately from songs, for example `config/patches/warm-pad.yaml`
- The current schema uses explicit typed `nodes` and `connections`
- Patch files may also declare `modulations.<name>` entries with `source`, `target`, `destination`, and `amount`, which compile into the runtime modulation matrix
- Oscillator and noise nodes may also declare an inline `amp_envelope` block as a shortcut for per-layer amplitude shaping
- Output nodes may declare `lfo1_rate_hz`; when it is positive, patch `lfo1` runs autonomously at that rate, and when it is omitted or `0`, the previous vibrato/mod-wheel-coupled behavior is preserved
- Filter nodes may declare `layer_target`; use `all` (default) to keep the current global filter behavior, or point at an active oscillator/noise node such as `osc1` to filter only that compiled layer while the others stay dry
- Supported node types in this first pass are `oscillator`, `noise`, `envelope`, `filter`, and `output`
- The compiler lowers those graphs into the current runtime `Instrument` type
- Signal graphs are intentionally constrained to the current engine model: up to 4 signal layers, at most one compiled filter stage in the active path, and no global/shared FX routing yet
- Supported modulation sources currently match the runtime engine surface: `lfo1`, `amp_envelope`, `filter_envelope`, `key_track`, `channel_aftertouch`, and `poly_aftertouch`
- Supported modulation destinations currently target the active compiled graph only: signal-layer `pitch_cents`, the active filter's `filter_cutoff_oct`, and the output node's `amp_gain`
- Patch modulation routes are validated at load time and capped at 8 routes per instrument to match the current render loop budget
- A signal layer may use either an inline `amp_envelope` block or an explicit `amp_envelope` connection, but not both
- Filter `layer_target` values are validated against the active compiled graph, so Studio and the engine will reject targets that do not point at an active oscillator/noise node
- Song YAML can now point at external patch files:
  - explicit pattern instruments may set `instruments.<name>.patch` or `instruments.<name>.patch_file`
  - generated song-intent roles may set `song.patches.<role>` or `song.patches.<role>.patch`
  - supported generated roles currently match the arranger layers: `drums`, `bass`, `pad`, `arp`, and `lead`
- Patch paths are resolved relative to the loaded song file, so song folders can carry their own patch assets
- Runtime playback, live genre switching, and the note-preview shell all preserve those patch overrides instead of falling back to the baked-in GM-style defaults

This loader/compiler now powers the first studio patch browser/editor pass, including a graph-canvas patcher; the next step is improving higher-level sound-design ergonomics on top of these external patch definitions.

## Timeline song YAML

The recommended schema is now a higher-level song-intent format:
- `song.genre` selects an arranger pack (currently `electronic`, `ambient`, `blackmetal`, and `cinematic`)
- `song.mood` provides the base mood target for arrangement and conductor weighting
- `song.tempo_bpm`, `song.beats_per_bar`, optional `song.beat_unit` provide section defaults
- `song.form` is a comma-separated section order such as `intro,verse,chorus,bridge,outro`
- `sections.<name>` defines `bars_per_phrase`, `phrase_count`, optional `mood`, `feel`, plus:
  - `chords: Am,F,C,G` for one chord per bar in the phrase
  - or `chord_regions.<change>.start_beat` + `symbol` for beat-level chord changes within the phrase
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

  verse:
    bars_per_phrase: 2
    phrase_count: 1
    chord_regions:
      change_1:
        start_beat: 0
        symbol: Am
      change_2:
        start_beat: 2
        symbol: F
      change_3:
        start_beat: 6
        symbol: G
```

For the new schema, the arranger generates drums, bass, pad, arp, and lead layers in Haskell from genre + mood + harmony/melody, and drum fills still honor the existing generated-fill system. Basslines and accompaniment now use chord-aware support tones, melody-guided extension tones, approach notes into upcoming roots, and smoother pad voicings so the generated harmony reacts to both the progression and the authored melodic anchor instead of repeating a fixed root/fifth loop. While the timeline is playing, `genre <name|default>` swaps the active arranger pack for newly generated bars, so you can move between supported genres live.

The older deterministic per-instrument schema is still supported:
- `song.mode: cue|drone`
- `song.lookahead_bars`
- `sections.<name>` with `tempo_bpm`, `beats_per_bar`, optional `beat_unit`, `bars_per_phrase`, `phrase_count`, `mood`, `feel`
- `instruments.<name>` with `instrument_id`, optional `patch` / `patch_file`, optional `amp`, `pan`, optional `velocity_variation` (0..1), and section patterns via either:
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
Deterministic replay is defined by authored song data + start frame/seed + the ordered stream of runtime control events. Given the same inputs, the arranger, conductor, and variation layer should emit the same generated bars and telemetry.
Transition observability is exposed via `PlayerEventTimelineTransition`, which reports section boundary transitions with from/to sections, reason, candidate weights (base + final), boundary timing, active mood/energy targets, and weighted-pick ticket info.
Retarget and lookahead observability are exposed via `PlayerEventTimelineRetargeted` and `PlayerEventTimelineLookahead`, so the game can explain why a musical reaction has or has not taken effect yet without guessing about the lookahead buffer.
The CLI now renders those telemetry events in a more human-readable explanation format during interactive timeline playback, instead of falling back to raw `show` output for transitions.

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
