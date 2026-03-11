import yaml from 'js-yaml';
import { midiToNoteName, noteNameToMidi } from './note';

const CHORD_ROOTS = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'] as const;
const CHORD_ROOT_INDEX = new Map<string, number>([
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

type ScaleDegreeDefinition = {
  id: string;
  label: string;
  semitone: number;
  suffix: string;
};

export type HarmonicMode = 'major' | 'minor';

export type ChordDegreeOption = {
  id: string;
  label: string;
  symbol: string;
};

export type ChordPreset = {
  id: string;
  label: string;
  modes: readonly HarmonicMode[];
  degrees: readonly string[];
};

const MAJOR_DEGREES: readonly ScaleDegreeDefinition[] = [
  { id: 'I', label: 'I', semitone: 0, suffix: '' },
  { id: 'ii', label: 'ii', semitone: 2, suffix: 'm' },
  { id: 'iii', label: 'iii', semitone: 4, suffix: 'm' },
  { id: 'IV', label: 'IV', semitone: 5, suffix: '' },
  { id: 'V', label: 'V', semitone: 7, suffix: '' },
  { id: 'vi', label: 'vi', semitone: 9, suffix: 'm' },
  { id: 'vii-dim', label: 'vii dim', semitone: 11, suffix: 'dim' },
] as const;

const MINOR_DEGREES: readonly ScaleDegreeDefinition[] = [
  { id: 'i', label: 'i', semitone: 0, suffix: 'm' },
  { id: 'III', label: 'III', semitone: 3, suffix: '' },
  { id: 'iv', label: 'iv', semitone: 5, suffix: 'm' },
  { id: 'v', label: 'v', semitone: 7, suffix: 'm' },
  { id: 'VI', label: 'VI', semitone: 8, suffix: '' },
  { id: 'VII', label: 'VII', semitone: 10, suffix: '' },
  { id: 'ii-dim', label: 'ii dim', semitone: 2, suffix: 'dim' },
] as const;

export const CHORD_KEY_OPTIONS = [...CHORD_ROOTS] as const;

export const CHORD_PRESETS: readonly ChordPreset[] = [
  { id: 'pop-lift', label: 'Pop lift', modes: ['major'], degrees: ['I', 'V', 'vi', 'IV'] },
  { id: 'anthem', label: 'Anthem', modes: ['major'], degrees: ['vi', 'IV', 'I', 'V'] },
  { id: 'sunrise', label: 'Sunrise', modes: ['major'], degrees: ['I', 'iii', 'vi', 'IV'] },
  { id: 'minor-rise', label: 'Minor rise', modes: ['minor'], degrees: ['i', 'VI', 'III', 'VII'] },
  { id: 'dusk', label: 'Dusk', modes: ['minor'], degrees: ['i', 'iv', 'VI', 'VII'] },
  { id: 'shadow-loop', label: 'Shadow loop', modes: ['minor'], degrees: ['i', 'VII', 'VI', 'VII'] },
] as const;

export type MelodyNote = {
  id: string;
  beat: number;
  note: number;
  duration: number;
  velocity: number;
};

export type SongSection = {
  name: string;
  barsPerPhrase: number;
  phraseCount: number;
  tempoBpm: number | null;
  beatsPerBar: number | null;
  beatUnit: number | null;
  mood: string;
  feel: string;
  chords: string[];
  melody: MelodyNote[];
};

export type SongDocument = {
  song: {
    genre: string;
    mood: string;
    tempoBpm: number;
    beatsPerBar: number;
    beatUnit: number;
  };
  sections: SongSection[];
};

function degreeDefinitionsForMode(mode: HarmonicMode): readonly ScaleDegreeDefinition[] {
  return mode === 'major' ? MAJOR_DEGREES : MINOR_DEGREES;
}

function normalizeChordRoot(root: string): string | null {
  const normalized = root.trim().toUpperCase();
  const index = CHORD_ROOT_INDEX.get(normalized);
  return index === undefined ? null : CHORD_ROOTS[index];
}

function parseChordSymbol(symbol: string): { root: string; suffix: string; bass: string | null } | null {
  const match = /^([A-Ga-g])([#bB]?)([^/]*?)(?:\/([A-Ga-g])([#bB]?))?$/.exec(symbol.trim());
  if (!match) {
    return null;
  }

  const [, rootLetter, rootAccidental, suffix, bassLetter, bassAccidental] = match;
  const root = normalizeChordRoot(`${rootLetter}${rootAccidental}`);
  if (!root) {
    return null;
  }

  const bass = bassLetter ? normalizeChordRoot(`${bassLetter}${bassAccidental ?? ''}`) : null;
  return {
    root,
    suffix: suffix.trim(),
    bass,
  };
}

function transposeChordRoot(root: string, semitones: number): string | null {
  const normalized = normalizeChordRoot(root);
  if (!normalized) {
    return null;
  }
  const index = CHORD_ROOT_INDEX.get(normalized.toUpperCase());
  if (index === undefined) {
    return null;
  }
  const nextIndex = (((index + semitones) % 12) + 12) % 12;
  return CHORD_ROOTS[nextIndex];
}

function chordQualityFromSuffix(suffix: string): 'major' | 'minor' | 'diminished' {
  const lowered = suffix.trim().toLowerCase();
  if (lowered.startsWith('m') && !lowered.startsWith('maj')) {
    return 'minor';
  }
  if (lowered.includes('dim') || lowered.includes('o')) {
    return 'diminished';
  }
  return 'major';
}

function formatRomanQuality(label: string, quality: 'major' | 'minor' | 'diminished'): string {
  const match = /^([b#]*)([ivIV]+)(.*)$/.exec(label);
  if (!match) {
    return label;
  }

  const [, accidental, roman, suffix] = match;
  const casedRoman = quality === 'major' ? roman.toUpperCase() : roman.toLowerCase();
  return quality === 'diminished'
    ? `${accidental}${casedRoman}${suffix} dim`
    : `${accidental}${casedRoman}${suffix}`;
}

function chromaticRomanLabel(interval: number, mode: HarmonicMode): string {
  const major = ['I', 'bII', 'ii', 'bIII', 'iii', 'IV', '#IV', 'V', 'bVI', 'vi', 'bVII', 'vii'];
  const minor = ['i', 'bII', 'ii', 'III', '#III', 'iv', '#iv', 'v', 'VI', '#VI', 'VII', 'vii'];
  return (mode === 'major' ? major : minor)[interval] ?? '?';
}

function sanitizeChordList(chords: string[]): string[] {
  return chords.map((chord) => chord.trim()).filter(Boolean);
}

type RawSongFile = {
  song?: Record<string, unknown>;
  sections?: Record<string, Record<string, unknown>>;
  instruments?: unknown;
};

function makeId(prefix: string): string {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `${prefix}-${crypto.randomUUID()}`;
  }
  return `${prefix}-${Math.random().toString(36).slice(2, 10)}`;
}

export function createNoteId(): string {
  return makeId('note');
}

export function normalizeSectionName(name: string): string {
  const normalized = name
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
  return normalized || 'section';
}

function asFiniteNumber(value: unknown, fallback: number): number {
  if (typeof value === 'number' && Number.isFinite(value)) {
    return value;
  }
  if (typeof value === 'string' && value.trim() !== '') {
    const parsed = Number(value);
    if (Number.isFinite(parsed)) {
      return parsed;
    }
  }
  return fallback;
}

function asOptionalNumber(value: unknown): number | null {
  if (value === undefined || value === null || value === '') {
    return null;
  }
  const parsed = asFiniteNumber(value, Number.NaN);
  return Number.isNaN(parsed) ? null : parsed;
}

function asText(value: unknown, fallback = ''): string {
  return typeof value === 'string' ? value.trim() : fallback;
}

function parseCsvText(value: unknown): string[] {
  if (Array.isArray(value)) {
    return value.map((entry) => String(entry).trim()).filter(Boolean);
  }
  if (typeof value === 'string') {
    return value
      .split(',')
      .map((entry) => entry.trim())
      .filter(Boolean);
  }
  return [];
}

function parseMelody(value: unknown): MelodyNote[] {
  if (typeof value !== 'string' || value.trim() === '') {
    return [];
  }

  return value
    .split(',')
    .map((token) => token.trim())
    .filter(Boolean)
    .map((token) => {
      const [beatText, noteText, durationText, velocityText] = token.split('/');
      const midi = noteNameToMidi(noteText) ?? 60;
      return {
        id: createNoteId(),
        beat: asFiniteNumber(beatText, 0),
        note: midi,
        duration: Math.max(0.25, asFiniteNumber(durationText, 1)),
        velocity: Math.min(1, Math.max(0, asFiniteNumber(velocityText, 0.7))),
      };
    })
    .sort((left, right) => left.beat - right.beat || left.note - right.note);
}

function trimFloat(value: number): string {
  const rounded = Math.round(value * 1000) / 1000;
  return String(rounded);
}

function formatMelody(notes: MelodyNote[]): string | undefined {
  if (notes.length === 0) {
    return undefined;
  }

  return notes
    .slice()
    .sort((left, right) => left.beat - right.beat || left.note - right.note)
    .map((note) => {
      const beat = trimFloat(note.beat);
      const duration = trimFloat(note.duration);
      const velocity = trimFloat(note.velocity);
      return `${beat}/${midiToNoteName(note.note)}/${duration}/${velocity}`;
    })
    .join(',');
}

export function createDefaultSong(): SongDocument {
  return {
    song: {
      genre: 'electronic',
      mood: 'dramatic',
      tempoBpm: 100,
      beatsPerBar: 4,
      beatUnit: 4,
    },
    sections: [
      {
        name: 'intro',
        barsPerPhrase: 4,
        phraseCount: 1,
        tempoBpm: null,
        beatsPerBar: null,
        beatUnit: null,
        mood: 'anticipatory',
        feel: 'sparse',
        chords: ['Am(add9)', 'Fmaj7', 'Dm7', 'Esus4'],
        melody: [
          { id: createNoteId(), beat: 2, note: 69, duration: 1, velocity: 0.54 },
          { id: createNoteId(), beat: 3, note: 72, duration: 0.75, velocity: 0.58 },
          { id: createNoteId(), beat: 3.75, note: 76, duration: 0.25, velocity: 0.62 },
        ],
      },
      {
        name: 'verse',
        barsPerPhrase: 4,
        phraseCount: 2,
        tempoBpm: null,
        beatsPerBar: null,
        beatUnit: null,
        mood: 'restless',
        feel: 'driving',
        chords: ['Am', 'F', 'C', 'G'],
        melody: [
          { id: createNoteId(), beat: 0, note: 69, duration: 0.5, velocity: 0.6 },
          { id: createNoteId(), beat: 1, note: 72, duration: 0.5, velocity: 0.62 },
          { id: createNoteId(), beat: 2, note: 76, duration: 1, velocity: 0.65 },
          { id: createNoteId(), beat: 6, note: 74, duration: 0.5, velocity: 0.58 },
        ],
      },
      {
        name: 'chorus',
        barsPerPhrase: 4,
        phraseCount: 2,
        tempoBpm: null,
        beatsPerBar: null,
        beatUnit: null,
        mood: 'wide',
        feel: 'anthemic',
        chords: ['F', 'G', 'Am', 'C'],
        melody: [
          { id: createNoteId(), beat: 0, note: 72, duration: 1, velocity: 0.72 },
          { id: createNoteId(), beat: 1.5, note: 76, duration: 0.5, velocity: 0.76 },
          { id: createNoteId(), beat: 2, note: 79, duration: 1, velocity: 0.82 },
          { id: createNoteId(), beat: 6, note: 77, duration: 0.5, velocity: 0.75 },
        ],
      },
    ],
  };
}

export function buildChordPalette(keyRoot: string, mode: HarmonicMode): ChordDegreeOption[] {
  const normalizedRoot = normalizeChordRoot(keyRoot) ?? 'C';
  const rootIndex = CHORD_ROOT_INDEX.get(normalizedRoot.toUpperCase()) ?? 0;

  return degreeDefinitionsForMode(mode).map((degree) => ({
    id: degree.id,
    label: degree.label,
    symbol: `${CHORD_ROOTS[(rootIndex + degree.semitone) % 12]}${degree.suffix}`,
  }));
}

export function availableChordPresets(mode: HarmonicMode): ChordPreset[] {
  return CHORD_PRESETS.filter((preset) => preset.modes.includes(mode));
}

export function applyChordPreset(keyRoot: string, mode: HarmonicMode, presetId: string): string[] {
  const palette = buildChordPalette(keyRoot, mode);
  const paletteById = new Map(palette.map((degree) => [degree.id, degree.symbol]));
  const preset = availableChordPresets(mode).find((candidate) => candidate.id === presetId);
  if (!preset) {
    return [];
  }

  return preset.degrees
    .map((degreeId) => paletteById.get(degreeId))
    .filter((value): value is string => typeof value === 'string');
}

export function transposeChordSymbol(symbol: string, semitones: number): string {
  const parsed = parseChordSymbol(symbol);
  if (!parsed) {
    return symbol;
  }

  const root = transposeChordRoot(parsed.root, semitones);
  const bass = parsed.bass ? transposeChordRoot(parsed.bass, semitones) : null;
  if (!root) {
    return symbol;
  }

  return `${root}${parsed.suffix}${bass ? `/${bass}` : ''}`;
}

export function transposeChordList(chords: string[], semitones: number): string[] {
  return sanitizeChordList(chords.map((chord) => transposeChordSymbol(chord, semitones)));
}

export function analyzeChordRoman(chordSymbol: string, keyRoot: string, mode: HarmonicMode): string {
  const parsed = parseChordSymbol(chordSymbol);
  const normalizedKey = normalizeChordRoot(keyRoot);
  if (!parsed || !normalizedKey) {
    return chordSymbol;
  }

  const chordIndex = CHORD_ROOT_INDEX.get(parsed.root.toUpperCase());
  const keyIndex = CHORD_ROOT_INDEX.get(normalizedKey.toUpperCase());
  if (chordIndex === undefined || keyIndex === undefined) {
    return chordSymbol;
  }

  const interval = (((chordIndex - keyIndex) % 12) + 12) % 12;
  const diatonicMatch = degreeDefinitionsForMode(mode).find((degree) => degree.semitone === interval);
  const label = diatonicMatch?.label ?? chromaticRomanLabel(interval, mode);
  return formatRomanQuality(label, chordQualityFromSuffix(parsed.suffix));
}

export function inferChordToolContext(chords: string[]): { keyRoot: string; mode: HarmonicMode } {
  const firstChord = sanitizeChordList(chords)[0];
  const parsed = firstChord ? parseChordSymbol(firstChord) : null;
  if (!parsed) {
    return { keyRoot: 'C', mode: 'major' };
  }

  return {
    keyRoot: parsed.root,
    mode: chordQualityFromSuffix(parsed.suffix) === 'minor' ? 'minor' : 'major',
  };
}

export function buildSectionLoopDocument(document: SongDocument, sectionName: string): SongDocument {
  const section = document.sections.find((entry) => entry.name === sectionName);
  if (!section) {
    return document;
  }

  return {
    song: { ...document.song },
    sections: [
      {
        ...section,
        chords: [...section.chords],
        melody: section.melody.map((note) => ({ ...note })),
      },
    ],
  };
}

export function parseSongText(contents: string): SongDocument {
  const parsed = (yaml.load(contents) ?? {}) as RawSongFile;
  if (!parsed.song || typeof parsed.song !== 'object') {
    throw new Error('YAML is missing a top-level song block.');
  }
  if (parsed.instruments) {
    throw new Error(
      'Idou Studio currently supports the high-level song-intent YAML format, not the explicit per-instrument schema.',
    );
  }

  const song = parsed.song;
  const genre = asText(song.genre, 'electronic');
  const mood = asText(song.mood, 'dramatic');
  const tempoBpm = asFiniteNumber(song.tempo_bpm, 100);
  const beatsPerBar = Math.max(1, Math.round(asFiniteNumber(song.beats_per_bar, 4)));
  const beatUnit = Math.max(1, Math.round(asFiniteNumber(song.beat_unit, 4)));

  const sectionsObject = parsed.sections ?? {};
  const knownNames = Object.keys(sectionsObject);
  const form = parseCsvText(song.form).filter((name) => knownNames.includes(name));
  const orderedNames =
    form.length > 0 ? [...form, ...knownNames.filter((name) => !form.includes(name))] : knownNames;

  const sections = orderedNames.map((name) => {
    const section = sectionsObject[name] ?? {};
    return {
      name: normalizeSectionName(name),
      barsPerPhrase: Math.max(1, Math.round(asFiniteNumber(section.bars_per_phrase, 4))),
      phraseCount: Math.max(1, Math.round(asFiniteNumber(section.phrase_count, 1))),
      tempoBpm: asOptionalNumber(section.tempo_bpm),
      beatsPerBar: asOptionalNumber(section.beats_per_bar),
      beatUnit: asOptionalNumber(section.beat_unit),
      mood: asText(section.mood),
      feel: asText(section.feel),
      chords: parseCsvText(section.chords),
      melody: parseMelody(section.melody),
    } satisfies SongSection;
  });

  if (sections.length === 0) {
    throw new Error('YAML does not define any sections.');
  }

  return {
    song: { genre, mood, tempoBpm, beatsPerBar, beatUnit },
    sections,
  };
}

export function serializeSong(document: SongDocument): string {
  const normalizedSections = document.sections.map((section) => ({
    ...section,
    name: normalizeSectionName(section.name),
  }));

  const root = {
    song: {
      genre: document.song.genre,
      mood: document.song.mood,
      tempo_bpm: document.song.tempoBpm,
      beats_per_bar: document.song.beatsPerBar,
      beat_unit: document.song.beatUnit,
      form: normalizedSections.map((section) => section.name).join(','),
    },
    sections: normalizedSections.reduce<Record<string, Record<string, unknown>>>((acc, section) => {
      const payload: Record<string, unknown> = {
        bars_per_phrase: section.barsPerPhrase,
        phrase_count: section.phraseCount,
      };
      if (section.tempoBpm !== null) payload.tempo_bpm = section.tempoBpm;
      if (section.beatsPerBar !== null) payload.beats_per_bar = section.beatsPerBar;
      if (section.beatUnit !== null) payload.beat_unit = section.beatUnit;
      if (section.mood) payload.mood = section.mood;
      if (section.feel) payload.feel = section.feel;
      if (section.chords.length > 0) payload.chords = section.chords.join(',');
      const melody = formatMelody(section.melody);
      if (melody) payload.melody = melody;
      acc[section.name] = payload;
      return acc;
    }, {}),
  };

  return yaml.dump(root, {
    noRefs: true,
    lineWidth: -1,
    sortKeys: false,
  });
}

export function sectionTotalBeats(document: SongDocument, section: SongSection): number {
  const beatsPerBar = section.beatsPerBar ?? document.song.beatsPerBar;
  return Math.max(1, section.barsPerPhrase * section.phraseCount * beatsPerBar);
}

export function validateSong(document: SongDocument): string[] {
  const issues: string[] = [];
  const names = new Set<string>();

  if (!document.song.genre.trim()) {
    issues.push('Song genre is required.');
  }
  if (!document.song.mood.trim()) {
    issues.push('Song mood is required.');
  }
  if (document.sections.length === 0) {
    issues.push('Add at least one section.');
  }

  document.sections.forEach((section) => {
    const normalizedName = normalizeSectionName(section.name);
    if (names.has(normalizedName)) {
      issues.push(`Section name "${normalizedName}" is duplicated.`);
    }
    names.add(normalizedName);

    const sanitizedChords = sanitizeChordList(section.chords);
    if (sanitizedChords.length === 0) {
      issues.push(`Section "${normalizedName}" should define at least one chord.`);
    }

    const totalBeats = sectionTotalBeats(document, section);
    section.melody.forEach((note) => {
      if (note.beat < 0) {
        issues.push(`Section "${normalizedName}" has a melody note before beat 0.`);
      }
      if (note.beat + note.duration > totalBeats + 0.001) {
        issues.push(`Section "${normalizedName}" has a melody note extending past the phrase length.`);
      }
    });
  });

  return issues;
}
