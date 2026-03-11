import yaml from 'js-yaml';
import { midiToNoteName, noteNameToMidi } from './note';

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
        id: makeId('note'),
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
          { id: makeId('note'), beat: 2, note: 69, duration: 1, velocity: 0.54 },
          { id: makeId('note'), beat: 3, note: 72, duration: 0.75, velocity: 0.58 },
          { id: makeId('note'), beat: 3.75, note: 76, duration: 0.25, velocity: 0.62 },
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
          { id: makeId('note'), beat: 0, note: 69, duration: 0.5, velocity: 0.6 },
          { id: makeId('note'), beat: 1, note: 72, duration: 0.5, velocity: 0.62 },
          { id: makeId('note'), beat: 2, note: 76, duration: 1, velocity: 0.65 },
          { id: makeId('note'), beat: 6, note: 74, duration: 0.5, velocity: 0.58 },
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
          { id: makeId('note'), beat: 0, note: 72, duration: 1, velocity: 0.72 },
          { id: makeId('note'), beat: 1.5, note: 76, duration: 0.5, velocity: 0.76 },
          { id: makeId('note'), beat: 2, note: 79, duration: 1, velocity: 0.82 },
          { id: makeId('note'), beat: 6, note: 77, duration: 0.5, velocity: 0.75 },
        ],
      },
    ],
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

    if (section.chords.length === 0) {
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
