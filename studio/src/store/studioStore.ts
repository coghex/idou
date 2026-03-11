import { create } from 'zustand';
import type { MelodyNote, SongDocument, SongSection } from '../lib/song';
import { createDefaultSong, createNoteId, normalizeSectionName, sectionTotalBeats } from '../lib/song';
import { midiToNoteName } from '../lib/note';

type PlaybackSnapshot = {
  running: boolean;
  transportState: 'stopped' | 'playing' | 'paused';
  stagedPath: string | null;
  loopSectionName: string | null;
  logLines: string[];
};

type EditorSnapshot = {
  document: SongDocument;
  selectedSectionName: string;
  selectedNoteId: string | null;
};

type ClipboardNote = Pick<MelodyNote, 'beat' | 'note' | 'duration' | 'velocity'>;

type StudioState = {
  document: SongDocument;
  currentPath: string | null;
  dirty: boolean;
  selectedSectionName: string;
  selectedNoteId: string | null;
  playback: PlaybackSnapshot;
  statusMessage: string;
  savedDocumentSignature: string;
  historyPast: EditorSnapshot[];
  historyFuture: EditorSnapshot[];
  clipboardNote: ClipboardNote | null;
  loadDocument: (document: SongDocument, currentPath: string | null) => void;
  newDocument: () => void;
  setCurrentPath: (currentPath: string | null) => void;
  markSaved: () => void;
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
  undo: () => void;
  redo: () => void;
  copySelectedNote: () => void;
  cutSelectedNote: () => void;
  pasteClipboard: () => void;
  setPlayback: (playback: PlaybackSnapshot) => void;
};

const MAX_HISTORY_ENTRIES = 100;

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

function cloneNote(note: MelodyNote): MelodyNote {
  return { ...note };
}

function cloneSection(section: SongSection): SongSection {
  return {
    ...section,
    chords: [...section.chords],
    melody: section.melody.map(cloneNote),
  };
}

function cloneDocument(document: SongDocument): SongDocument {
  return {
    song: { ...document.song },
    sections: document.sections.map(cloneSection),
  };
}

function cloneSnapshot(snapshot: EditorSnapshot): EditorSnapshot {
  return {
    document: cloneDocument(snapshot.document),
    selectedSectionName: snapshot.selectedSectionName,
    selectedNoteId: snapshot.selectedNoteId,
  };
}

function documentSignature(document: SongDocument): string {
  return JSON.stringify(document);
}

function normalizeSelection(
  document: SongDocument,
  selectedSectionName: string,
  selectedNoteId: string | null,
): Pick<EditorSnapshot, 'selectedSectionName' | 'selectedNoteId'> {
  const selectedSection =
    document.sections.find((section) => section.name === selectedSectionName) ?? document.sections[0] ?? null;
  const nextSectionName = selectedSection?.name ?? '';
  const nextNoteId =
    selectedSection && selectedNoteId && selectedSection.melody.some((note) => note.id === selectedNoteId)
      ? selectedNoteId
      : null;
  return {
    selectedSectionName: nextSectionName,
    selectedNoteId: nextNoteId,
  };
}

function snapshotFromState(state: Pick<StudioState, 'document' | 'selectedSectionName' | 'selectedNoteId'>): EditorSnapshot {
  return {
    document: cloneDocument(state.document),
    selectedSectionName: state.selectedSectionName,
    selectedNoteId: state.selectedNoteId,
  };
}

function resetEditorState(document: SongDocument, currentPath: string | null, statusMessage: string) {
  const cloned = cloneDocument(document);
  const selection = normalizeSelection(cloned, cloned.sections[0]?.name ?? '', null);
  return {
    document: cloned,
    currentPath,
    dirty: false,
    selectedSectionName: selection.selectedSectionName,
    selectedNoteId: selection.selectedNoteId,
    savedDocumentSignature: documentSignature(cloned),
    historyPast: [],
    historyFuture: [],
    statusMessage,
  };
}

function applySnapshot(
  state: StudioState,
  snapshot: EditorSnapshot,
  options?: {
    pushHistory?: boolean;
    statusMessage?: string;
    historyFuture?: EditorSnapshot[];
    historyPast?: EditorSnapshot[];
  },
) {
  const document = cloneDocument(snapshot.document);
  const selection = normalizeSelection(document, snapshot.selectedSectionName, snapshot.selectedNoteId);
  const nextSnapshot = {
    document,
    selectedSectionName: selection.selectedSectionName,
    selectedNoteId: selection.selectedNoteId,
  } satisfies EditorSnapshot;
  const historyPast = options?.historyPast ?? state.historyPast;
  const historyFuture = options?.historyFuture ?? (options?.pushHistory ? [] : state.historyFuture);
  const pushedPast =
    options?.pushHistory
      ? [...historyPast, snapshotFromState(state)].slice(-MAX_HISTORY_ENTRIES)
      : historyPast;

  return {
    document: nextSnapshot.document,
    selectedSectionName: nextSnapshot.selectedSectionName,
    selectedNoteId: nextSnapshot.selectedNoteId,
    dirty: documentSignature(nextSnapshot.document) !== state.savedDocumentSignature,
    historyPast: pushedPast,
    historyFuture,
    statusMessage: options?.statusMessage ?? state.statusMessage,
  };
}

function selectedSectionFromState(state: StudioState): SongSection | null {
  return state.document.sections.find((section) => section.name === state.selectedSectionName) ?? state.document.sections[0] ?? null;
}

function selectedNoteFromState(state: StudioState, section: SongSection | null): MelodyNote | null {
  if (!section || !state.selectedNoteId) {
    return null;
  }
  return section.melody.find((note) => note.id === state.selectedNoteId) ?? null;
}

function initialState(): Omit<
  StudioState,
  | 'loadDocument'
  | 'newDocument'
  | 'setCurrentPath'
  | 'markSaved'
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
  | 'undo'
  | 'redo'
  | 'copySelectedNote'
  | 'cutSelectedNote'
  | 'pasteClipboard'
  | 'setPlayback'
> {
  const document = createDefaultSong();
  const cloned = cloneDocument(document);
  return {
    document: cloned,
    currentPath: null,
    dirty: false,
    selectedSectionName: cloned.sections[0]?.name ?? 'intro',
    selectedNoteId: null,
    playback: {
      running: false,
      transportState: 'stopped',
      stagedPath: null,
      loopSectionName: null,
      logLines: [],
    },
    statusMessage: 'Ready.',
    savedDocumentSignature: documentSignature(cloned),
    historyPast: [],
    historyFuture: [],
    clipboardNote: null,
  };
}

export const useStudioStore = create<StudioState>((set, get) => ({
  ...initialState(),
  loadDocument: (document, currentPath) =>
    set((state) => ({
      ...resetEditorState(document, currentPath, currentPath ? `Opened ${currentPath}` : 'Opened document.'),
      clipboardNote: state.clipboardNote,
      playback: state.playback,
    })),
  newDocument: () =>
    set((state) => ({
      ...initialState(),
      clipboardNote: state.clipboardNote,
      statusMessage: 'Created a new song document.',
      playback: state.playback,
    })),
  setCurrentPath: (currentPath) => set({ currentPath }),
  markSaved: () =>
    set((state) => ({
      dirty: false,
      savedDocumentSignature: documentSignature(state.document),
    })),
  setStatusMessage: (statusMessage) => set({ statusMessage }),
  setSongField: (field, value) =>
    set((state) =>
      applySnapshot(
        state,
        {
          ...snapshotFromState(state),
          document: {
            ...cloneDocument(state.document),
            song: {
              ...state.document.song,
              [field]: value,
            },
          },
        },
        { pushHistory: true },
      ),
    ),
  addSection: () =>
    set((state) => {
      const nextSection = defaultSection(state.document.sections);
      const document = cloneDocument(state.document);
      document.sections.push(nextSection);
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: nextSection.name,
          selectedNoteId: null,
        },
        { pushHistory: true, statusMessage: `Added section ${nextSection.name}.` },
      );
    }),
  updateSection: (sectionName, updater) =>
    set((state) => {
      let nextSelectedSectionName = state.selectedSectionName;
      const document = cloneDocument(state.document);
      document.sections = document.sections.map((section) => {
        if (section.name !== sectionName) {
          return section;
        }
        const updated = updater(section);
        if (state.selectedSectionName === sectionName) {
          nextSelectedSectionName = updated.name;
        }
        return updated;
      });
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: nextSelectedSectionName,
          selectedNoteId: state.selectedNoteId,
        },
        { pushHistory: true },
      );
    }),
  removeSection: (sectionName) =>
    set((state) => {
      const document = cloneDocument(state.document);
      document.sections = document.sections.filter((section) => section.name !== sectionName);
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: document.sections[0]?.name ?? '',
          selectedNoteId: null,
        },
        { pushHistory: true, statusMessage: `Removed section ${sectionName}.` },
      );
    }),
  moveSection: (sectionName, direction) =>
    set((state) => {
      const document = cloneDocument(state.document);
      const index = document.sections.findIndex((section) => section.name === sectionName);
      const nextIndex = index + direction;
      if (index < 0 || nextIndex < 0 || nextIndex >= document.sections.length) {
        return state;
      }
      const [section] = document.sections.splice(index, 1);
      document.sections.splice(nextIndex, 0, section);
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: state.selectedSectionName,
          selectedNoteId: state.selectedNoteId,
        },
        { pushHistory: true },
      );
    }),
  selectSection: (selectedSectionName) => set({ selectedSectionName, selectedNoteId: null }),
  setSelectedNoteId: (selectedNoteId) => set({ selectedNoteId }),
  upsertNote: (sectionName, note) =>
    set((state) => {
      const document = cloneDocument(state.document);
      document.sections = document.sections.map((section) => {
        if (section.name !== sectionName) {
          return section;
        }
        const melody = section.melody.some((entry) => entry.id === note.id)
          ? section.melody.map((entry) => (entry.id === note.id ? { ...note } : entry))
          : [...section.melody, { ...note }];
        return {
          ...section,
          melody: melody.sort((left, right) => left.beat - right.beat || left.note - right.note),
        };
      });
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: sectionName,
          selectedNoteId: note.id,
        },
        { pushHistory: true },
      );
    }),
  removeNote: (sectionName, noteId) =>
    set((state) => {
      const document = cloneDocument(state.document);
      document.sections = document.sections.map((section) =>
        section.name === sectionName
          ? { ...section, melody: section.melody.filter((note) => note.id !== noteId) }
          : section,
      );
      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: state.selectedSectionName,
          selectedNoteId: state.selectedNoteId === noteId ? null : state.selectedNoteId,
        },
        { pushHistory: true },
      );
    }),
  undo: () =>
    set((state) => {
      const previous = state.historyPast[state.historyPast.length - 1];
      if (!previous) {
        return state;
      }
      return applySnapshot(state, previous, {
        historyPast: state.historyPast.slice(0, -1),
        historyFuture: [snapshotFromState(state), ...state.historyFuture].slice(0, MAX_HISTORY_ENTRIES),
        statusMessage: 'Undo.',
      });
    }),
  redo: () =>
    set((state) => {
      const next = state.historyFuture[0];
      if (!next) {
        return state;
      }
      return applySnapshot(state, next, {
        historyPast: [...state.historyPast, snapshotFromState(state)].slice(-MAX_HISTORY_ENTRIES),
        historyFuture: state.historyFuture.slice(1),
        statusMessage: 'Redo.',
      });
    }),
  copySelectedNote: () =>
    set((state) => {
      const section = selectedSectionFromState(state);
      const note = selectedNoteFromState(state, section);
      if (!note) {
        return state;
      }
      return {
        clipboardNote: {
          beat: note.beat,
          note: note.note,
          duration: note.duration,
          velocity: note.velocity,
        },
        statusMessage: `Copied ${midiToNoteName(note.note)}.`,
      };
    }),
  cutSelectedNote: () =>
    set((state) => {
      const section = selectedSectionFromState(state);
      const note = selectedNoteFromState(state, section);
      if (!section || !note) {
        return state;
      }
      const document = cloneDocument(state.document);
      document.sections = document.sections.map((entry) =>
        entry.name === section.name
          ? { ...entry, melody: entry.melody.filter((candidate) => candidate.id !== note.id) }
          : entry,
      );
      return {
        ...applySnapshot(
          state,
          {
            document,
            selectedSectionName: section.name,
            selectedNoteId: null,
          },
          { pushHistory: true, statusMessage: `Cut ${midiToNoteName(note.note)}.` },
        ),
        clipboardNote: {
          beat: note.beat,
          note: note.note,
          duration: note.duration,
          velocity: note.velocity,
        },
      };
    }),
  pasteClipboard: () =>
    set((state) => {
      const section = selectedSectionFromState(state);
      if (!section || !state.clipboardNote) {
        return state;
      }

      const selectedNote = selectedNoteFromState(state, section);
      const totalBeats = sectionTotalBeats(state.document, section);
      const duration = Math.max(0.25, Math.min(state.clipboardNote.duration, totalBeats));
      const anchorBeat = selectedNote ? selectedNote.beat + selectedNote.duration : state.clipboardNote.beat;
      const beat = Math.max(0, Math.min(Math.max(0, totalBeats - duration), anchorBeat));
      const pastedNote: MelodyNote = {
        id: createNoteId(),
        beat,
        note: state.clipboardNote.note,
        duration,
        velocity: state.clipboardNote.velocity,
      };

      const document = cloneDocument(state.document);
      document.sections = document.sections.map((entry) =>
        entry.name === section.name
          ? {
              ...entry,
              melody: [...entry.melody, pastedNote].sort((left, right) => left.beat - right.beat || left.note - right.note),
            }
          : entry,
      );

      return applySnapshot(
        state,
        {
          document,
          selectedSectionName: section.name,
          selectedNoteId: pastedNote.id,
        },
        { pushHistory: true, statusMessage: `Pasted ${midiToNoteName(pastedNote.note)}.` },
      );
    }),
  setPlayback: (playback) => set({ playback }),
}));
