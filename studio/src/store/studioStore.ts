import { create } from 'zustand';
import type { MelodyNote, SongDocument, SongSection } from '../lib/song';
import { createDefaultSong, normalizeSectionName } from '../lib/song';

type PlaybackSnapshot = {
  running: boolean;
  stagedPath: string | null;
  logLines: string[];
};

type StudioState = {
  document: SongDocument;
  currentPath: string | null;
  dirty: boolean;
  selectedSectionName: string;
  selectedNoteId: string | null;
  playback: PlaybackSnapshot;
  statusMessage: string;
  loadDocument: (document: SongDocument, currentPath: string | null) => void;
  newDocument: () => void;
  setCurrentPath: (currentPath: string | null) => void;
  setDirty: (dirty: boolean) => void;
  setStatusMessage: (statusMessage: string) => void;
  setSongField: <K extends keyof SongDocument['song']>(field: K, value: SongDocument['song'][K]) => void;
  addSection: () => void;
  updateSection: (sectionName: string, updater: (section: SongSection) => SongSection) => void;
  removeSection: (sectionName: string) => void;
  moveSection: (sectionName: string, direction: -1 | 1) => void;
  selectSection: (sectionName: string) => void;
  setSelectedNoteId: (selectedNoteId: string | null) => void;
  upsertNote: (sectionName: string, note: MelodyNote) => void;
  removeNote: (sectionName: string, noteId: string) => void;
  setPlayback: (playback: PlaybackSnapshot) => void;
};

function pickUniqueSectionName(existing: SongSection[], baseName = 'section'): string {
  const existingNames = new Set(existing.map((section) => normalizeSectionName(section.name)));
  let candidate = normalizeSectionName(baseName);
  let suffix = 2;
  while (existingNames.has(candidate)) {
    candidate = `${normalizeSectionName(baseName)}-${suffix}`;
    suffix += 1;
  }
  return candidate;
}

function defaultSection(existing: SongSection[]): SongSection {
  return {
    name: pickUniqueSectionName(existing),
    barsPerPhrase: 4,
    phraseCount: 1,
    tempoBpm: null,
    beatsPerBar: null,
    beatUnit: null,
    mood: '',
    feel: '',
    chords: ['Am', 'F', 'C', 'G'],
    melody: [],
  };
}

function initialState(): Omit<
  StudioState,
  | 'loadDocument'
  | 'newDocument'
  | 'setCurrentPath'
  | 'setDirty'
  | 'setStatusMessage'
  | 'setSongField'
  | 'addSection'
  | 'updateSection'
  | 'removeSection'
  | 'moveSection'
  | 'selectSection'
  | 'setSelectedNoteId'
  | 'upsertNote'
  | 'removeNote'
  | 'setPlayback'
> {
  const document = createDefaultSong();
  return {
    document,
    currentPath: null,
    dirty: false,
    selectedSectionName: document.sections[0]?.name ?? 'intro',
    selectedNoteId: null,
    playback: {
      running: false,
      stagedPath: null,
      logLines: [],
    },
    statusMessage: 'Ready.',
  };
}

export const useStudioStore = create<StudioState>((set) => ({
  ...initialState(),
  loadDocument: (document, currentPath) =>
    set({
      document,
      currentPath,
      dirty: false,
      selectedSectionName: document.sections[0]?.name ?? '',
      selectedNoteId: null,
      statusMessage: currentPath ? `Opened ${currentPath}` : 'Opened document.',
    }),
  newDocument: () => set({ ...initialState(), statusMessage: 'Created a new song document.' }),
  setCurrentPath: (currentPath) => set({ currentPath }),
  setDirty: (dirty) => set({ dirty }),
  setStatusMessage: (statusMessage) => set({ statusMessage }),
  setSongField: (field, value) =>
    set((state) => ({
      document: {
        ...state.document,
        song: {
          ...state.document.song,
          [field]: value,
        },
      },
      dirty: true,
    })),
  addSection: () =>
    set((state) => {
      const nextSection = defaultSection(state.document.sections);
      return {
        document: {
          ...state.document,
          sections: [...state.document.sections, nextSection],
        },
        selectedSectionName: nextSection.name,
        selectedNoteId: null,
        dirty: true,
        statusMessage: `Added section ${nextSection.name}.`,
      };
    }),
  updateSection: (sectionName, updater) =>
    set((state) => {
      let nextSelectedSectionName = state.selectedSectionName;
      const sections = state.document.sections.map((section) => {
        if (section.name !== sectionName) {
          return section;
        }
        const updated = updater(section);
        if (state.selectedSectionName === sectionName) {
          nextSelectedSectionName = updated.name;
        }
        return updated;
      });
      return {
        document: {
          ...state.document,
          sections,
        },
        selectedSectionName: nextSelectedSectionName,
        dirty: true,
      };
    }),
  removeSection: (sectionName) =>
    set((state) => {
      const remaining = state.document.sections.filter((section) => section.name !== sectionName);
      const nextSelection = remaining[0]?.name ?? '';
      return {
        document: {
          ...state.document,
          sections: remaining,
        },
        selectedSectionName: nextSelection,
        selectedNoteId: null,
        dirty: true,
        statusMessage: `Removed section ${sectionName}.`,
      };
    }),
  moveSection: (sectionName, direction) =>
    set((state) => {
      const sections = [...state.document.sections];
      const index = sections.findIndex((section) => section.name === sectionName);
      const nextIndex = index + direction;
      if (index < 0 || nextIndex < 0 || nextIndex >= sections.length) {
        return state;
      }
      const [section] = sections.splice(index, 1);
      sections.splice(nextIndex, 0, section);
      return {
        document: {
          ...state.document,
          sections,
        },
        dirty: true,
      };
    }),
  selectSection: (selectedSectionName) => set({ selectedSectionName, selectedNoteId: null }),
  setSelectedNoteId: (selectedNoteId) => set({ selectedNoteId }),
  upsertNote: (sectionName, note) =>
    set((state) => ({
      document: {
        ...state.document,
        sections: state.document.sections.map((section) => {
          if (section.name !== sectionName) {
            return section;
          }
          const melody = section.melody.some((entry) => entry.id === note.id)
            ? section.melody.map((entry) => (entry.id === note.id ? note : entry))
            : [...section.melody, note];
          return {
            ...section,
            melody: melody.sort((left, right) => left.beat - right.beat || left.note - right.note),
          };
        }),
      },
      selectedNoteId: note.id,
      dirty: true,
    })),
  removeNote: (sectionName, noteId) =>
    set((state) => ({
      document: {
        ...state.document,
        sections: state.document.sections.map((section) =>
          section.name === sectionName
            ? { ...section, melody: section.melody.filter((note) => note.id !== noteId) }
            : section,
        ),
      },
      selectedNoteId: state.selectedNoteId === noteId ? null : state.selectedNoteId,
      dirty: true,
    })),
  setPlayback: (playback) => set({ playback }),
}));
