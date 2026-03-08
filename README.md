# Idou

Haskell FFI to miniaudio for playback.

Runtime audio settings live in `config/audio.yaml` under the `audio:` key.

MIDI playback currently supports note on/off, tempo changes, General MIDI-style Program Change patch switching, pitch bend (default range ±2 semitones), and controller messages for mod wheel (CC1), channel volume (CC7), pan (CC10), expression (CC11), sustain pedal (CC64), all sound off (CC120), reset all controllers (CC121), and all notes off (CC123). Channel 10 uses a percussion-style synth patch.

Run with:

```bash
cabal run idou -- path/to/file.mid
```
