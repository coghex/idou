const NOTE_NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const NOTE_INDEX = new Map<string, number>([
  ['C', 0],
  ['B#', 0],
  ['C#', 1],
  ['DB', 1],
  ['D', 2],
  ['D#', 3],
  ['EB', 3],
  ['E', 4],
  ['FB', 4],
  ['F', 5],
  ['E#', 5],
  ['F#', 6],
  ['GB', 6],
  ['G', 7],
  ['G#', 8],
  ['AB', 8],
  ['A', 9],
  ['A#', 10],
  ['BB', 10],
  ['B', 11],
  ['CB', 11],
]);

export function midiToNoteName(midi: number): string {
  const clamped = Math.max(0, Math.min(127, Math.round(midi)));
  const pitchClass = clamped % 12;
  const octave = Math.floor(clamped / 12) - 1;
  return `${NOTE_NAMES[pitchClass]}${octave}`;
}

export function noteNameToMidi(name: string): number | null {
  const match = /^([A-Ga-g])([#bB]?)(-?\d+)$/.exec(name.trim());
  if (!match) {
    return null;
  }

  const [, letter, accidental, octaveText] = match;
  const pitchName = `${letter.toUpperCase()}${accidental.toUpperCase()}`;
  const pitchClass = NOTE_INDEX.get(pitchName);
  if (pitchClass === undefined) {
    return null;
  }

  const octave = Number.parseInt(octaveText, 10);
  if (Number.isNaN(octave)) {
    return null;
  }

  const midi = (octave + 1) * 12 + pitchClass;
  if (midi < 0 || midi > 127) {
    return null;
  }
  return midi;
}

export function quantizeBeat(beat: number, step = 0.25): number {
  return Math.round(beat / step) * step;
}

export function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}
