import { useCallback, useEffect, useMemo, useState } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { yaml as yamlLanguage } from '@codemirror/lang-yaml';
import { invoke } from '@tauri-apps/api/core';
import MelodyGrid, { type OverlayNote } from './components/MelodyGrid';
import { midiToNoteName, noteNameToMidi } from './lib/note';
import {
  buildSectionLoopDocument,
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

function transportLabel(playback: PlaybackSnapshot): string {
  if (!playback.running) {
    return 'Engine preview idle';
  }
  if (playback.transportState === 'playing') {
    return playback.loopSectionName ? `Looping section ${playback.loopSectionName}` : 'Playing song preview';
  }
  if (playback.transportState === 'paused') {
    return 'Preview paused';
  }
  return 'Preview loaded';
}

function App() {
  const document = useStudioStore((state) => state.document);
  const currentPath = useStudioStore((state) => state.currentPath);
  const dirty = useStudioStore((state) => state.dirty);
  const selectedSectionName = useStudioStore((state) => state.selectedSectionName);
  const selectedNoteId = useStudioStore((state) => state.selectedNoteId);
  const playback = useStudioStore((state) => state.playback) as PlaybackSnapshot;
  const statusMessage = useStudioStore((state) => state.statusMessage);
  const loadDocument = useStudioStore((state) => state.loadDocument);
  const newDocument = useStudioStore((state) => state.newDocument);
  const setCurrentPath = useStudioStore((state) => state.setCurrentPath);
  const setDirty = useStudioStore((state) => state.setDirty);
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
  const setPlayback = useStudioStore((state) => state.setPlayback);

  const [timelinePreview, setTimelinePreview] = useState<TimelinePreview>({ sections: [] });
  const [accompanimentLoading, setAccompanimentLoading] = useState(false);
  const [accompanimentDirty, setAccompanimentDirty] = useState(true);
  const [accompanimentError, setAccompanimentError] = useState<string | null>(null);

  const selectedSection =
    document.sections.find((section) => section.name === selectedSectionName) ?? document.sections[0] ?? null;
  const selectedNote = selectedSection?.melody.find((note) => note.id === selectedNoteId) ?? null;
  const yamlPreview = useMemo(() => serializeSong(document), [document]);
  const validationIssues = useMemo(() => validateSong(document), [document]);
  const selectedTimelineSection =
    timelinePreview.sections.find((section) => section.name === selectedSection?.name) ?? null;

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

  async function openSong() {
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
  }

  async function saveSong(forceDialog = false) {
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
      setDirty(false);
      setStatusMessage(`Saved ${response.path}`);
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }

  const playPreview = useCallback(
    async (loopSelectedSection: boolean) => {
      const previewDocument =
        loopSelectedSection && selectedSection ? buildSectionLoopDocument(document, selectedSection.name) : document;
      const previewText = serializeSong(previewDocument);
      const suggestedName = loopSelectedSection && selectedSection ? selectedSection.name : selectedSection?.name ?? 'preview';

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
            : 'Started preview playback through the headless Idou engine.',
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

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.code !== 'Space' || isEditableTarget(event.target)) {
        return;
      }
      event.preventDefault();
      void togglePlayback();
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [togglePlayback]);

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

  const totalBeats = selectedSection ? sectionTotalBeats(document, selectedSection) : 0;

  const overlayNotes = useMemo<OverlayNote[]>(() => {
    if (!selectedTimelineSection) {
      return [];
    }

    return selectedTimelineSection.notes.map((note, index) => ({
      id: `overlay-${note.instrumentName}-${index}`,
      beat: note.startBeat,
      note: note.key,
      duration: Math.max(0.1, note.durationBeats),
      color: instrumentColor(note.instrumentName),
      accentColor: instrumentAccent(note.instrumentName),
    }));
  }, [selectedTimelineSection]);

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
        <div>
          <h1>Idou Studio</h1>
          <p>Visual song-intent editor, loop-preview tool, and generated accompaniment viewer for your headless synth engine.</p>
        </div>
        <div className="toolbar-actions">
          <button onClick={() => newDocument()}>New</button>
          <button onClick={() => void openSong()}>Open</button>
          <button onClick={() => void saveSong(false)}>Save</button>
          <button onClick={() => void saveSong(true)}>Save As</button>
          <button className="primary" onClick={() => void playPreview(false)}>
            Play song
          </button>
          <button onClick={() => void playPreview(true)} disabled={!selectedSection}>
            Loop section
          </button>
          <button onClick={() => void togglePlayback()}>{playback.running && playback.transportState === 'playing' ? 'Pause (Space)' : 'Play / Resume (Space)'}</button>
          <button onClick={() => void stopPreview()}>Stop</button>
          <button onClick={() => void refreshAccompaniment()} disabled={accompanimentLoading}>
            {accompanimentLoading ? 'Refreshing…' : accompanimentDirty ? 'Refresh layers *' : 'Refresh layers'}
          </button>
        </div>
      </header>

      <div className="status-strip">
        <span>{dirty ? 'Unsaved changes' : 'Saved'}</span>
        <span>{currentPath ?? 'Untitled song'}</span>
        <span>{transportLabel(playback)}</span>
        <span>Space toggles play/pause</span>
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
              <button onClick={() => addSection()}>Add section</button>
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
                    <button onClick={() => moveSection(section.name, -1)}>Up</button>
                    <button onClick={() => moveSection(section.name, 1)}>Down</button>
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
                  <h2>Section editor</h2>
                  <div className="inline-actions">
                    <button onClick={() => moveSection(selectedSection.name, -1)}>Move up</button>
                    <button onClick={() => moveSection(selectedSection.name, 1)}>Move down</button>
                    <button onClick={() => void playPreview(true)}>Loop this section</button>
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
                      placeholder="Use song default"
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
                      placeholder="Use song default"
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
                      placeholder="Use song default"
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
                  <h2>Melody + accompaniment</h2>
                  <span>{totalBeats} beats visible</span>
                </div>
                <MelodyGrid
                  beatsPerBar={selectedSection.beatsPerBar ?? document.song.beatsPerBar}
                  totalBeats={totalBeats}
                  notes={selectedSection.melody}
                  overlayNotes={overlayNotes}
                  selectedNoteId={selectedNoteId}
                  onSelectNote={setSelectedNoteId}
                  onUpsertNote={(note) => upsertNote(selectedSection.name, note)}
                />

                <div className="generated-strip">
                  <div>
                    <strong>Generated accompaniment</strong>
                    <span className="hint">
                      {accompanimentLoading
                        ? ' Refreshing generated layers...'
                        : accompanimentDirty
                          ? ' Overlay is stale until you refresh it or start playback.'
                          : ' Overlay matches the latest generated arrangement.'}
                    </span>
                  </div>
                  {selectedTimelineSection ? (
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
                    <div className="hint">No generated layer preview loaded for this section yet.</div>
                  )}
                  {accompanimentError ? <div className="validation-inline">{accompanimentError}</div> : null}
                </div>

                {selectedNote ? (
                  <div className="note-editor">
                    <h3>Selected note</h3>
                    <div className="field-grid compact-grid">
                      <label>
                        Start beat
                        <input
                          type="number"
                          step={0.25}
                          min={0}
                          max={Math.max(0, totalBeats - 0.25)}
                          value={selectedNote.beat}
                          onChange={(event) =>
                            updateSelectedNote((note) => ({ ...note, beat: Number(event.target.value) || 0 }))
                          }
                        />
                      </label>
                      <label>
                        Duration
                        <input
                          type="number"
                          step={0.25}
                          min={0.25}
                          max={totalBeats}
                          value={selectedNote.duration}
                          onChange={(event) =>
                            updateSelectedNote((note) => ({ ...note, duration: Number(event.target.value) || 0.25 }))
                          }
                        />
                      </label>
                      <label>
                        Note name
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
                    <div className="inline-actions">
                      <button className="danger" onClick={() => removeNote(selectedSection.name, selectedNote.id)}>
                        Delete note
                      </button>
                    </div>
                  </div>
                ) : (
                  <div className="note-editor empty-state">Select a melody note to edit its exact values.</div>
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
              <h2>YAML preview</h2>
            </div>
            <CodeMirror value={yamlPreview} height="340px" extensions={[yamlLanguage()]} editable={false} />
          </section>
          <section>
            <div className="panel-header">
              <h2>Validation</h2>
            </div>
            {validationIssues.length === 0 ? (
              <div className="validation-ok">No validation issues detected.</div>
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
              <h2>Layer stats</h2>
            </div>
            {selectedTimelineSection ? (
              <div className="layer-stats">
                <div>{selectedTimelineSection.notes.length} generated notes</div>
                <div>{selectedTimelineSection.beatsPerBar} beats per bar</div>
                <div>{selectedTimelineSection.totalBeats} total beats</div>
              </div>
            ) : (
              <div className="hint">Refresh generated layers to inspect the current section arrangement.</div>
            )}
          </section>
          <section>
            <div className="panel-header">
              <h2>Engine logs</h2>
            </div>
            <div className="log-panel">
              {playback.logLines.length === 0 ? 'No engine output yet.' : playback.logLines.join('\n')}
            </div>
            {playback.stagedPath ? <div className="hint">Preview file: {playback.stagedPath}</div> : null}
          </section>
        </aside>
      </main>

      <footer className="footer-strip">{statusMessage}</footer>
    </div>
  );
}

export default App;
