import { useCallback, useEffect, useMemo, useState } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { yaml as yamlLanguage } from '@codemirror/lang-yaml';
import { invoke } from '@tauri-apps/api/core';
import MelodyGrid, { type OverlayNote } from './components/MelodyGrid';
import { clamp, midiToNoteName, noteNameToMidi, quantizeBeat } from './lib/note';
import {
  buildSectionLoopDocument,
  createNoteId,
  normalizeSectionName,
  parseSongText,
  sectionTotalBeats,
  serializeSong,
  validateSong,
} from './lib/song';
import type { MelodyNote, SongSection } from './lib/song';
import { useStudioStore } from './store/studioStore';

type FilePayload = {
  path: string;
  contents: string;
};

type SaveSongResponse = {
  path: string;
};

type PlaybackSnapshot = {
  running: boolean;
  transportState: 'stopped' | 'playing' | 'paused';
  stagedPath: string | null;
  loopSectionName: string | null;
  logLines: string[];
};

type TimelinePreview = {
  sections: TimelinePreviewSection[];
};

type TimelinePreviewSection = {
  name: string;
  beatsPerBar: number;
  totalBeats: number;
  notes: TimelinePreviewNote[];
};

type TimelinePreviewNote = {
  instrumentId: number;
  instrumentName: string;
  key: number;
  velocity: number;
  amp: number;
  pan: number;
  barIndex: number;
  startBeat: number;
  durationBeats: number;
};

const SNAP_OPTIONS = [
  { label: '1/4', value: 1 },
  { label: '1/8', value: 0.5 },
  { label: '1/16', value: 0.25 },
  { label: '1/32', value: 0.125 },
] as const;

const AUDITION_INSTRUMENT_OPTIONS = [
  { label: 'Lead', value: 'lead' },
  { label: 'Pad', value: 'pad' },
  { label: 'Arp', value: 'arp' },
  { label: 'Bass', value: 'bass' },
] as const;

const INSTRUMENT_COLORS: Record<string, string> = {
  drums: 'rgba(168, 85, 247, 0.26)',
  bass: 'rgba(34, 197, 94, 0.26)',
  pad: 'rgba(59, 130, 246, 0.23)',
  arp: 'rgba(236, 72, 153, 0.24)',
  lead: 'rgba(249, 115, 22, 0.25)',
  'auto-drums': 'rgba(168, 85, 247, 0.26)',
};

const INSTRUMENT_ACCENTS: Record<string, string> = {
  drums: 'rgba(196, 181, 253, 0.82)',
  bass: 'rgba(134, 239, 172, 0.82)',
  pad: 'rgba(147, 197, 253, 0.82)',
  arp: 'rgba(249, 168, 212, 0.82)',
  lead: 'rgba(253, 186, 116, 0.82)',
  'auto-drums': 'rgba(196, 181, 253, 0.82)',
};

function isEditableTarget(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) {
    return false;
  }
  return Boolean(target.closest('input, textarea, select, button, .cm-editor')) || target.isContentEditable;
}

function fallbackInstrumentColor(name: string): string {
  const palette = [
    'rgba(14, 165, 233, 0.22)',
    'rgba(99, 102, 241, 0.22)',
    'rgba(234, 88, 12, 0.22)',
    'rgba(16, 185, 129, 0.22)',
    'rgba(244, 63, 94, 0.22)',
  ];
  let hash = 0;
  for (const char of name) {
    hash = (hash * 31 + char.charCodeAt(0)) >>> 0;
  }
  return palette[hash % palette.length];
}

function instrumentColor(name: string): string {
  return INSTRUMENT_COLORS[name] ?? fallbackInstrumentColor(name);
}

function instrumentAccent(name: string): string {
  return INSTRUMENT_ACCENTS[name] ?? 'rgba(226, 232, 240, 0.72)';
}

function basename(path: string | null): string {
  if (!path) {
    return 'Untitled song';
  }
  const normalized = path.replace(/\\/g, '/');
  const parts = normalized.split('/');
  return parts[parts.length - 1] || 'Untitled song';
}

function transportLabel(playback: PlaybackSnapshot): string {
  if (!playback.running) {
    return 'Stopped';
  }
  if (playback.transportState === 'playing') {
    return playback.loopSectionName ? `Loop: ${playback.loopSectionName}` : 'Playing';
  }
  if (playback.transportState === 'paused') {
    return 'Paused';
  }
  return 'Loaded';
}

function App() {
  const document = useStudioStore((state) => state.document);
  const currentPath = useStudioStore((state) => state.currentPath);
  const dirty = useStudioStore((state) => state.dirty);
  const selectedSectionName = useStudioStore((state) => state.selectedSectionName);
  const selectedNoteId = useStudioStore((state) => state.selectedNoteId);
  const canUndo = useStudioStore((state) => state.historyPast.length > 0);
  const canRedo = useStudioStore((state) => state.historyFuture.length > 0);
  const hasClipboard = useStudioStore((state) => state.clipboardNote !== null);
  const playback = useStudioStore((state) => state.playback) as PlaybackSnapshot;
  const statusMessage = useStudioStore((state) => state.statusMessage);
  const loadDocument = useStudioStore((state) => state.loadDocument);
  const newDocument = useStudioStore((state) => state.newDocument);
  const setCurrentPath = useStudioStore((state) => state.setCurrentPath);
  const markSaved = useStudioStore((state) => state.markSaved);
  const setStatusMessage = useStudioStore((state) => state.setStatusMessage);
  const setSongField = useStudioStore((state) => state.setSongField);
  const addSection = useStudioStore((state) => state.addSection);
  const updateSection = useStudioStore((state) => state.updateSection);
  const removeSection = useStudioStore((state) => state.removeSection);
  const moveSection = useStudioStore((state) => state.moveSection);
  const selectSection = useStudioStore((state) => state.selectSection);
  const setSelectedNoteId = useStudioStore((state) => state.setSelectedNoteId);
  const upsertNote = useStudioStore((state) => state.upsertNote);
  const removeNote = useStudioStore((state) => state.removeNote);
  const undo = useStudioStore((state) => state.undo);
  const redo = useStudioStore((state) => state.redo);
  const copySelectedNote = useStudioStore((state) => state.copySelectedNote);
  const cutSelectedNote = useStudioStore((state) => state.cutSelectedNote);
  const pasteClipboard = useStudioStore((state) => state.pasteClipboard);
  const setPlayback = useStudioStore((state) => state.setPlayback);

  const [timelinePreview, setTimelinePreview] = useState<TimelinePreview>({ sections: [] });
  const [accompanimentLoading, setAccompanimentLoading] = useState(false);
  const [accompanimentDirty, setAccompanimentDirty] = useState(true);
  const [accompanimentError, setAccompanimentError] = useState<string | null>(null);
  const [snapStep, setSnapStep] = useState<number>(0.25);
  const [beatWidth, setBeatWidth] = useState<number>(42);
  const [showAccompaniment, setShowAccompaniment] = useState(true);
  const [auditionInstrument, setAuditionInstrument] = useState<string>('lead');

  const selectedSection =
    document.sections.find((section) => section.name === selectedSectionName) ?? document.sections[0] ?? null;
  const selectedNote = selectedSection?.melody.find((note) => note.id === selectedNoteId) ?? null;
  const yamlPreview = useMemo(() => serializeSong(document), [document]);
  const validationIssues = useMemo(() => validateSong(document), [document]);
  const selectedTimelineSection =
    timelinePreview.sections.find((section) => section.name === selectedSection?.name) ?? null;
  const totalBeats = selectedSection ? sectionTotalBeats(document, selectedSection) : 0;
  const activeBeatsPerBar = selectedSection?.beatsPerBar ?? document.song.beatsPerBar;
  const currentFileLabel = useMemo(() => basename(currentPath), [currentPath]);
  const currentSnapLabel =
    SNAP_OPTIONS.find((option) => option.value === snapStep)?.label ?? `${snapStep.toFixed(3)} beat`;

  useEffect(() => {
    setAccompanimentDirty(true);
  }, [yamlPreview]);

  useEffect(() => {
    const timer = window.setInterval(async () => {
      try {
        const snapshot = await invoke<PlaybackSnapshot>('playback_snapshot');
        setPlayback(snapshot);
      } catch (error) {
        console.error(error);
      }
    }, 1000);

    return () => window.clearInterval(timer);
  }, [setPlayback]);

  const refreshAccompaniment = useCallback(
    async (contents = yamlPreview, suggestedName = selectedSection?.name ?? 'preview') => {
      try {
        setAccompanimentLoading(true);
        setAccompanimentError(null);
        const preview = await invoke<TimelinePreview>('dump_song_timeline', {
          request: {
            contents,
            suggestedName,
          },
        });
        setTimelinePreview(preview);
        setAccompanimentDirty(false);
      } catch (error) {
        setAccompanimentError(error instanceof Error ? error.message : String(error));
      } finally {
        setAccompanimentLoading(false);
      }
    },
    [selectedSection?.name, yamlPreview],
  );

  const openSong = useCallback(async () => {
    try {
      const payload = await invoke<FilePayload | null>('open_song_file');
      if (!payload) {
        return;
      }
      const parsed = parseSongText(payload.contents);
      loadDocument(parsed, payload.path);
      setTimelinePreview({ sections: [] });
      setAccompanimentError(null);
      setAccompanimentDirty(true);
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [loadDocument, setStatusMessage]);

  const saveSong = useCallback(
    async (forceDialog = false) => {
      try {
        const response = await invoke<SaveSongResponse | null>('save_song_file', {
          request: {
            path: forceDialog ? null : currentPath,
            contents: yamlPreview,
          },
        });
        if (!response) {
          return;
        }
        setCurrentPath(response.path);
        markSaved();
        setStatusMessage(`Saved ${response.path}`);
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [currentPath, markSaved, setCurrentPath, setStatusMessage, yamlPreview],
  );

  const playPreview = useCallback(
    async (loopSelectedSection: boolean) => {
      const previewDocument =
        loopSelectedSection && selectedSection ? buildSectionLoopDocument(document, selectedSection.name) : document;
      const previewText = serializeSong(previewDocument);
      const suggestedName =
        loopSelectedSection && selectedSection ? selectedSection.name : selectedSection?.name ?? 'preview';

      try {
        const snapshot = await invoke<PlaybackSnapshot>('play_song_preview', {
          request: {
            contents: previewText,
            suggestedName,
            loopSectionName: loopSelectedSection && selectedSection ? selectedSection.name : null,
          },
        });
        setPlayback(snapshot);
        void refreshAccompaniment(previewText, suggestedName);
        setStatusMessage(
          loopSelectedSection && selectedSection
            ? `Looping section ${selectedSection.name}.`
            : 'Started preview playback.',
        );
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [document, refreshAccompaniment, selectedSection, setPlayback, setStatusMessage],
  );

  const pausePreview = useCallback(async () => {
    try {
      const snapshot = await invoke<PlaybackSnapshot>('pause_song_preview');
      setPlayback(snapshot);
      setStatusMessage('Paused preview playback.');
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [setPlayback, setStatusMessage]);

  const resumePreview = useCallback(async () => {
    try {
      const snapshot = await invoke<PlaybackSnapshot>('resume_song_preview');
      setPlayback(snapshot);
      setStatusMessage('Resumed preview playback.');
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [setPlayback, setStatusMessage]);

  const stopPreview = useCallback(async () => {
    try {
      const snapshot = await invoke<PlaybackSnapshot>('stop_song_preview');
      setPlayback(snapshot);
      setStatusMessage('Stopped preview playback.');
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [setPlayback, setStatusMessage]);

  const togglePlayback = useCallback(async () => {
    if (playback.running && playback.transportState === 'playing') {
      await pausePreview();
    } else if (playback.running) {
      await resumePreview();
    } else {
      await playPreview(false);
    }
  }, [pausePreview, playPreview, playback.running, playback.transportState, resumePreview]);

  const previewPianoNote = useCallback(
    async (midi: number) => {
      try {
        await invoke('preview_song_note', {
          request: {
            genre: document.song.genre,
            instrumentName: auditionInstrument,
            midi,
          },
        });
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [auditionInstrument, document.song.genre, setStatusMessage],
  );

  const removeSelectedNote = useCallback(() => {
    if (!selectedSection || !selectedNote) {
      return;
    }
    removeNote(selectedSection.name, selectedNote.id);
    setStatusMessage(`Deleted ${midiToNoteName(selectedNote.note)}.`);
  }, [removeNote, selectedNote, selectedSection, setStatusMessage]);

  const duplicateSelectedNote = useCallback(() => {
    if (!selectedSection || !selectedNote) {
      return;
    }

    const nextBeat = clamp(
      quantizeBeat(selectedNote.beat + selectedNote.duration, snapStep),
      0,
      Math.max(0, totalBeats - selectedNote.duration),
    );
    const duplicated: MelodyNote = {
      ...selectedNote,
      id: createNoteId(),
      beat: nextBeat,
    };
    upsertNote(selectedSection.name, duplicated);
    setSelectedNoteId(duplicated.id);
    setStatusMessage(`Duplicated ${midiToNoteName(selectedNote.note)}.`);
  }, [selectedNote, selectedSection, setSelectedNoteId, setStatusMessage, snapStep, totalBeats, upsertNote]);

  const nudgeSelectedNote = useCallback(
    (beatDelta: number, noteDelta: number) => {
      if (!selectedSection || !selectedNote) {
        return;
      }

      const nextBeat = clamp(
        quantizeBeat(selectedNote.beat + beatDelta, snapStep),
        0,
        Math.max(0, totalBeats - selectedNote.duration),
      );
      const nextNote = clamp(selectedNote.note + noteDelta, 0, 127);
      upsertNote(selectedSection.name, {
        ...selectedNote,
        beat: nextBeat,
        note: nextNote,
      });
    },
    [selectedNote, selectedSection, snapStep, totalBeats, upsertNote],
  );

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      const modifier = event.metaKey || event.ctrlKey;

      if (modifier && event.code === 'KeyS') {
        event.preventDefault();
        void saveSong(false);
        return;
      }

      if (modifier && event.code === 'KeyO') {
        event.preventDefault();
        void openSong();
        return;
      }

      if (isEditableTarget(event.target)) {
        return;
      }

      if (modifier && event.code === 'KeyZ') {
        event.preventDefault();
        if (event.shiftKey) {
          redo();
        } else {
          undo();
        }
        return;
      }

      if (modifier && event.code === 'KeyY') {
        event.preventDefault();
        redo();
        return;
      }

      if (modifier && event.code === 'KeyC') {
        event.preventDefault();
        copySelectedNote();
        return;
      }

      if (modifier && event.code === 'KeyX') {
        event.preventDefault();
        cutSelectedNote();
        return;
      }

      if (modifier && event.code === 'KeyV') {
        event.preventDefault();
        pasteClipboard();
        return;
      }

      if (event.code === 'Space') {
        if (event.repeat) {
          return;
        }
        event.preventDefault();
        void togglePlayback();
        return;
      }

      if (modifier && event.code === 'KeyD') {
        if (event.repeat) {
          return;
        }
        event.preventDefault();
        duplicateSelectedNote();
        return;
      }

      switch (event.code) {
        case 'Delete':
        case 'Backspace':
          if (selectedNote) {
            event.preventDefault();
            removeSelectedNote();
          }
          break;
        case 'Escape':
          setSelectedNoteId(null);
          break;
        case 'ArrowLeft':
          if (selectedNote) {
            event.preventDefault();
            nudgeSelectedNote(-(event.shiftKey ? activeBeatsPerBar : snapStep), 0);
          }
          break;
        case 'ArrowRight':
          if (selectedNote) {
            event.preventDefault();
            nudgeSelectedNote(event.shiftKey ? activeBeatsPerBar : snapStep, 0);
          }
          break;
        case 'ArrowUp':
          if (selectedNote) {
            event.preventDefault();
            nudgeSelectedNote(0, event.shiftKey ? 12 : 1);
          }
          break;
        case 'ArrowDown':
          if (selectedNote) {
            event.preventDefault();
            nudgeSelectedNote(0, event.shiftKey ? -12 : -1);
          }
          break;
        default:
          break;
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [
    activeBeatsPerBar,
    duplicateSelectedNote,
    nudgeSelectedNote,
    openSong,
    copySelectedNote,
    cutSelectedNote,
    pasteClipboard,
    removeSelectedNote,
    redo,
    saveSong,
    selectedNote,
    setSelectedNoteId,
    snapStep,
    togglePlayback,
    undo,
  ]);

  function updateSelectedSection(updater: (section: SongSection) => SongSection) {
    if (!selectedSection) {
      return;
    }
    updateSection(selectedSection.name, updater);
  }

  function setSelectedSectionField<K extends keyof SongSection>(field: K, value: SongSection[K]) {
    updateSelectedSection(
      (section) =>
        ({
          ...section,
          [field]: field === 'name' ? normalizeSectionName(String(value)) : value,
        }) as SongSection,
    );
  }

  function updateSelectedNote(updater: (note: MelodyNote) => MelodyNote) {
    if (!selectedSection || !selectedNote) {
      return;
    }
    upsertNote(selectedSection.name, updater(selectedNote));
  }

  const overlayNotes = useMemo<OverlayNote[]>(() => {
    if (!showAccompaniment || !selectedTimelineSection) {
      return [];
    }

    return selectedTimelineSection.notes.map((note, index) => ({
      id: `overlay-${note.instrumentName}-${index}`,
      beat: note.startBeat,
      note: note.key,
      duration: Math.max(snapStep / 2, note.durationBeats),
      color: instrumentColor(note.instrumentName),
      accentColor: instrumentAccent(note.instrumentName),
    }));
  }, [selectedTimelineSection, showAccompaniment, snapStep]);

  const accompanimentLegend = useMemo(() => {
    if (!selectedTimelineSection) {
      return [];
    }
    const counts = new Map<string, number>();
    for (const note of selectedTimelineSection.notes) {
      counts.set(note.instrumentName, (counts.get(note.instrumentName) ?? 0) + 1);
    }
    return [...counts.entries()].map(([name, count]) => ({
      name,
      count,
      color: instrumentColor(name),
      accentColor: instrumentAccent(name),
    }));
  }, [selectedTimelineSection]);

  return (
    <div className="app-shell">
      <header className="topbar">
        <div className="toolbar-group">
          <div className="file-pill">
            <span className={dirty ? 'dirty-indicator dirty' : 'dirty-indicator'} />
            <span>{currentFileLabel}</span>
          </div>
          <button onClick={() => newDocument()}>New</button>
          <button onClick={() => void openSong()}>Open</button>
          <button onClick={() => void saveSong(false)}>Save</button>
          <button onClick={() => void saveSong(true)}>Save As</button>
          <button onClick={() => undo()} disabled={!canUndo}>
            Undo
          </button>
          <button onClick={() => redo()} disabled={!canRedo}>
            Redo
          </button>
        </div>

        <div className="toolbar-group transport-group">
          <button className="transport-button primary" title="Play song" onClick={() => void playPreview(false)}>
            ▶
          </button>
          <button className="transport-button" title="Pause or resume" onClick={() => void togglePlayback()}>
            {playback.running && playback.transportState === 'playing' ? '⏸' : '▶'}
          </button>
          <button className="transport-button" title="Stop" onClick={() => void stopPreview()}>
            ■
          </button>
          <button
            className={playback.loopSectionName ? 'transport-button active' : 'transport-button'}
            title="Loop selected section"
            onClick={() => void playPreview(true)}
            disabled={!selectedSection}
          >
            ↻
          </button>
        </div>

        <div className="toolbar-group toolbar-readout">
          <span className="status-chip">{transportLabel(playback)}</span>
          <span className="status-chip">{selectedSection ? selectedSection.name : 'no section'}</span>
          <span className="status-chip">{currentSnapLabel}</span>
        </div>
      </header>

      <div className="status-strip">
        <span>{dirty ? 'Unsaved' : 'Saved'}</span>
        <span>{showAccompaniment ? 'Layers visible' : 'Layers hidden'}</span>
        <span>{hasClipboard ? 'Clipboard armed' : 'Clipboard empty'}</span>
        <span>{selectedNote ? `Selected ${midiToNoteName(selectedNote.note)}` : 'No note selected'}</span>
      </div>

      <main className="workspace">
        <aside className="sidebar panel">
          <section>
            <div className="panel-header">
              <h2>Song</h2>
            </div>
            <div className="field-grid">
              <label>
                Genre
                <input value={document.song.genre} onChange={(event) => setSongField('genre', event.target.value)} />
              </label>
              <label>
                Mood
                <input value={document.song.mood} onChange={(event) => setSongField('mood', event.target.value)} />
              </label>
              <label>
                Tempo BPM
                <input
                  type="number"
                  min={20}
                  max={280}
                  value={document.song.tempoBpm}
                  onChange={(event) => setSongField('tempoBpm', Number(event.target.value) || 100)}
                />
              </label>
              <label>
                Beats / Bar
                <input
                  type="number"
                  min={1}
                  max={16}
                  value={document.song.beatsPerBar}
                  onChange={(event) => setSongField('beatsPerBar', Number(event.target.value) || 4)}
                />
              </label>
              <label>
                Beat Unit
                <input
                  type="number"
                  min={1}
                  max={16}
                  value={document.song.beatUnit}
                  onChange={(event) => setSongField('beatUnit', Number(event.target.value) || 4)}
                />
              </label>
            </div>
          </section>

          <section>
            <div className="panel-header">
              <h2>Sections</h2>
              <button onClick={() => addSection()}>Add</button>
            </div>
            <div className="section-list">
              {document.sections.map((section, index) => (
                <button
                  key={`${section.name}-${index}`}
                  className={section.name === selectedSectionName ? 'section-chip selected' : 'section-chip'}
                  onClick={() => selectSection(section.name)}
                >
                  <span>{section.name}</span>
                  <span className="section-chip-meta">{index + 1}</span>
                </button>
              ))}
            </div>
          </section>

          <section>
            <div className="panel-header">
              <h2>Form</h2>
            </div>
            <ol className="form-list">
              {document.sections.map((section) => (
                <li key={section.name}>
                  <span>{section.name}</span>
                  <div className="inline-actions">
                    <button onClick={() => moveSection(section.name, -1)}>↑</button>
                    <button onClick={() => moveSection(section.name, 1)}>↓</button>
                  </div>
                </li>
              ))}
            </ol>
          </section>
        </aside>

        <section className="editor-column">
          {selectedSection ? (
            <>
              <div className="panel section-editor">
                <div className="panel-header">
                  <h2>Section</h2>
                  <div className="inline-actions">
                    <button onClick={() => moveSection(selectedSection.name, -1)}>Move up</button>
                    <button onClick={() => moveSection(selectedSection.name, 1)}>Move down</button>
                    <button onClick={() => void playPreview(true)}>Loop section</button>
                    <button className="danger" onClick={() => removeSection(selectedSection.name)}>
                      Delete
                    </button>
                  </div>
                </div>

                <div className="field-grid section-fields">
                  <label>
                    Section name
                    <input value={selectedSection.name} onChange={(event) => setSelectedSectionField('name', event.target.value)} />
                  </label>
                  <label>
                    Bars / phrase
                    <input
                      type="number"
                      min={1}
                      max={16}
                      value={selectedSection.barsPerPhrase}
                      onChange={(event) => setSelectedSectionField('barsPerPhrase', Number(event.target.value) || 1)}
                    />
                  </label>
                  <label>
                    Phrase count
                    <input
                      type="number"
                      min={1}
                      max={16}
                      value={selectedSection.phraseCount}
                      onChange={(event) => setSelectedSectionField('phraseCount', Number(event.target.value) || 1)}
                    />
                  </label>
                  <label>
                    Override tempo
                    <input
                      type="number"
                      min={20}
                      max={280}
                      value={selectedSection.tempoBpm ?? ''}
                      placeholder="Song default"
                      onChange={(event) =>
                        setSelectedSectionField('tempoBpm', event.target.value === '' ? null : Number(event.target.value))
                      }
                    />
                  </label>
                  <label>
                    Override beats / bar
                    <input
                      type="number"
                      min={1}
                      max={16}
                      value={selectedSection.beatsPerBar ?? ''}
                      placeholder="Song default"
                      onChange={(event) =>
                        setSelectedSectionField('beatsPerBar', event.target.value === '' ? null : Number(event.target.value))
                      }
                    />
                  </label>
                  <label>
                    Override beat unit
                    <input
                      type="number"
                      min={1}
                      max={16}
                      value={selectedSection.beatUnit ?? ''}
                      placeholder="Song default"
                      onChange={(event) =>
                        setSelectedSectionField('beatUnit', event.target.value === '' ? null : Number(event.target.value))
                      }
                    />
                  </label>
                  <label>
                    Mood
                    <input value={selectedSection.mood} onChange={(event) => setSelectedSectionField('mood', event.target.value)} />
                  </label>
                  <label>
                    Feel
                    <input value={selectedSection.feel} onChange={(event) => setSelectedSectionField('feel', event.target.value)} />
                  </label>
                  <label className="full-width">
                    Chords
                    <input
                      value={selectedSection.chords.join(',')}
                      onChange={(event) =>
                        setSelectedSectionField(
                          'chords',
                          event.target.value
                            .split(',')
                            .map((entry) => entry.trim())
                            .filter(Boolean),
                        )
                      }
                    />
                  </label>
                </div>
              </div>

              <div className="panel melody-panel">
                <div className="panel-header">
                  <h2>Piano roll</h2>
                  <div className="toolbar-group compact-toolbar">
                    <button onClick={() => copySelectedNote()} disabled={!selectedNote}>
                      Copy
                    </button>
                    <button onClick={() => cutSelectedNote()} disabled={!selectedNote}>
                      Cut
                    </button>
                    <button onClick={() => pasteClipboard()} disabled={!hasClipboard}>
                      Paste
                    </button>
                    <button onClick={() => duplicateSelectedNote()} disabled={!selectedNote}>
                      Duplicate
                    </button>
                    <button onClick={() => removeSelectedNote()} disabled={!selectedNote}>
                      Delete
                    </button>
                  </div>
                </div>

                <div className="editor-toolbar">
                  <label className="toolbar-control">
                    <span>Snap</span>
                    <select value={snapStep} onChange={(event) => setSnapStep(Number(event.target.value))}>
                      {SNAP_OPTIONS.map((option) => (
                        <option key={option.label} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="toolbar-control">
                    <span>Audition</span>
                    <select value={auditionInstrument} onChange={(event) => setAuditionInstrument(event.target.value)}>
                      {AUDITION_INSTRUMENT_OPTIONS.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="toolbar-control zoom-control">
                    <span>Zoom</span>
                    <input
                      type="range"
                      min={24}
                      max={84}
                      step={2}
                      value={beatWidth}
                      onChange={(event) => setBeatWidth(Number(event.target.value))}
                    />
                  </label>
                  <button
                    className={showAccompaniment ? 'toggle-button active' : 'toggle-button'}
                    onClick={() => setShowAccompaniment((value) => !value)}
                  >
                    Layers
                  </button>
                  <button onClick={() => void refreshAccompaniment()} disabled={accompanimentLoading}>
                    {accompanimentLoading ? 'Refreshing…' : 'Refresh'}
                  </button>
                </div>

                <MelodyGrid
                  beatsPerBar={activeBeatsPerBar}
                  totalBeats={totalBeats}
                  beatWidth={beatWidth}
                  quantizeStep={snapStep}
                  notes={selectedSection.melody}
                  overlayNotes={overlayNotes}
                  selectedNoteId={selectedNoteId}
                  onSelectNote={setSelectedNoteId}
                  onUpsertNote={(note) => upsertNote(selectedSection.name, note)}
                  onPreviewNote={(midi) => void previewPianoNote(midi)}
                />

                <div className="generated-strip">
                  <div className="strip-head">
                    <strong>Generated layers</strong>
                    <span className="hint">
                      {accompanimentLoading
                        ? 'Refreshing...'
                        : accompanimentDirty
                          ? 'Refresh after edits'
                          : `${selectedTimelineSection?.notes.length ?? 0} notes`}
                    </span>
                  </div>
                  {selectedTimelineSection && showAccompaniment ? (
                    <div className="legend-list">
                      {accompanimentLegend.map((entry) => (
                        <span key={entry.name} className="legend-chip">
                          <span
                            className="legend-dot"
                            style={{ background: entry.color, borderColor: entry.accentColor }}
                          />
                          {entry.name} ({entry.count})
                        </span>
                      ))}
                    </div>
                  ) : (
                    <div className="hint">
                      {showAccompaniment ? 'No generated layer preview loaded for this section.' : 'Generated layers hidden.'}
                    </div>
                  )}
                  {accompanimentError ? <div className="validation-inline">{accompanimentError}</div> : null}
                </div>

                {selectedNote ? (
                  <div className="note-editor">
                    <div className="panel-header">
                      <h3>Selected note</h3>
                      <span className="hint">
                        {midiToNoteName(selectedNote.note)} · beat {selectedNote.beat}
                      </span>
                    </div>
                    <div className="field-grid compact-grid">
                      <label>
                        Start beat
                        <input
                          type="number"
                          step={snapStep}
                          min={0}
                          max={Math.max(0, totalBeats - snapStep)}
                          value={selectedNote.beat}
                          onChange={(event) =>
                            updateSelectedNote((note) => ({
                              ...note,
                              beat: clamp(
                                quantizeBeat(Number(event.target.value) || 0, snapStep),
                                0,
                                Math.max(0, totalBeats - note.duration),
                              ),
                            }))
                          }
                        />
                      </label>
                      <label>
                        Duration
                        <input
                          type="number"
                          step={snapStep}
                          min={snapStep}
                          max={totalBeats}
                          value={selectedNote.duration}
                          onChange={(event) =>
                            updateSelectedNote((note) => ({
                              ...note,
                              duration: clamp(
                                quantizeBeat(Number(event.target.value) || snapStep, snapStep),
                                snapStep,
                                Math.max(snapStep, totalBeats - note.beat),
                              ),
                            }))
                          }
                        />
                      </label>
                      <label>
                        Note
                        <input
                          value={midiToNoteName(selectedNote.note)}
                          onChange={(event) => {
                            const midi = noteNameToMidi(event.target.value);
                            if (midi !== null) {
                              updateSelectedNote((note) => ({ ...note, note: midi }));
                            }
                          }}
                        />
                      </label>
                      <label>
                        Velocity
                        <input
                          type="number"
                          step={0.01}
                          min={0}
                          max={1}
                          value={selectedNote.velocity}
                          onChange={(event) =>
                            updateSelectedNote((note) => ({ ...note, velocity: Number(event.target.value) || 0 }))
                          }
                        />
                      </label>
                    </div>
                    <div className="shortcut-strip">
                      <span>Click piano keys audition {auditionInstrument}</span>
                      <span>⌘/Ctrl+Z undo</span>
                      <span>⇧⌘/Ctrl+Z redo</span>
                      <span>⌘/Ctrl+C/X/V clipboard</span>
                      <span>Space play/pause</span>
                      <span>⌘/Ctrl+D duplicate</span>
                      <span>Delete remove</span>
                      <span>Arrows nudge</span>
                      <span>Shift+Arrows large nudge</span>
                    </div>
                  </div>
                ) : (
                  <div className="note-editor empty-state">Select a note, or click in the grid to create one.</div>
                )}
              </div>
            </>
          ) : (
            <div className="panel empty-state">Add a section to start editing.</div>
          )}
        </section>

        <aside className="sidebar panel right-sidebar">
          <section>
            <div className="panel-header">
              <h2>YAML</h2>
            </div>
            <CodeMirror value={yamlPreview} height="320px" extensions={[yamlLanguage()]} editable={false} />
          </section>
          <section>
            <div className="panel-header">
              <h2>Validation</h2>
            </div>
            {validationIssues.length === 0 ? (
              <div className="validation-ok">No validation issues.</div>
            ) : (
              <ul className="validation-list">
                {validationIssues.map((issue) => (
                  <li key={issue}>{issue}</li>
                ))}
              </ul>
            )}
          </section>
          <section>
            <div className="panel-header">
              <h2>Inspector</h2>
            </div>
            {selectedTimelineSection ? (
              <div className="layer-stats">
                <div>{selectedTimelineSection.notes.length} generated notes</div>
                <div>{selectedTimelineSection.beatsPerBar} beats / bar</div>
                <div>{selectedTimelineSection.totalBeats} visible beats</div>
              </div>
            ) : (
              <div className="hint">Refresh generated layers to inspect the current arrangement.</div>
            )}
          </section>
          <section>
            <div className="panel-header">
              <h2>Engine</h2>
            </div>
            <div className="log-panel">
              {playback.logLines.length === 0 ? 'No engine output yet.' : playback.logLines.join('\n')}
            </div>
          </section>
        </aside>
      </main>

      <footer className="footer-strip">
        <span>{statusMessage}</span>
        <span>⌘/Ctrl+S Save · ⌘/Ctrl+Z Undo · ⌘/Ctrl+C/X/V Clipboard · Space Play/Pause</span>
      </footer>
    </div>
  );
}

export default App;
