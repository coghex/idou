import { useEffect, useMemo } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { yaml as yamlLanguage } from '@codemirror/lang-yaml';
import { invoke } from '@tauri-apps/api/core';
import MelodyGrid from './components/MelodyGrid';
import { midiToNoteName, noteNameToMidi } from './lib/note';
import { normalizeSectionName, parseSongText, sectionTotalBeats, serializeSong, validateSong } from './lib/song';
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
  stagedPath: string | null;
  logLines: string[];
};

function App() {
  const document = useStudioStore((state) => state.document);
  const currentPath = useStudioStore((state) => state.currentPath);
  const dirty = useStudioStore((state) => state.dirty);
  const selectedSectionName = useStudioStore((state) => state.selectedSectionName);
  const selectedNoteId = useStudioStore((state) => state.selectedNoteId);
  const playback = useStudioStore((state) => state.playback);
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

  const selectedSection =
    document.sections.find((section) => section.name === selectedSectionName) ?? document.sections[0] ?? null;
  const selectedNote = selectedSection?.melody.find((note) => note.id === selectedNoteId) ?? null;
  const yamlPreview = useMemo(() => serializeSong(document), [document]);
  const validationIssues = useMemo(() => validateSong(document), [document]);

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

  async function openSong() {
    try {
      const payload = await invoke<FilePayload | null>('open_song_file');
      if (!payload) {
        return;
      }
      const parsed = parseSongText(payload.contents);
      loadDocument(parsed, payload.path);
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

  async function playPreview() {
    try {
      const snapshot = await invoke<PlaybackSnapshot>('play_song_preview', {
        request: {
          contents: yamlPreview,
          suggestedName: selectedSection?.name ?? 'preview',
        },
      });
      setPlayback(snapshot);
      setStatusMessage('Started preview playback through the headless Idou engine.');
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }

  async function stopPreview() {
    try {
      const snapshot = await invoke<PlaybackSnapshot>('stop_song_preview');
      setPlayback(snapshot);
      setStatusMessage('Stopped preview playback.');
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }

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

  return (
    <div className="app-shell">
      <header className="topbar">
        <div>
          <h1>Idou Studio</h1>
          <p>Visual song-intent editor and preview tool for your headless synth engine.</p>
        </div>
        <div className="toolbar-actions">
          <button onClick={() => newDocument()}>New</button>
          <button onClick={() => void openSong()}>Open</button>
          <button onClick={() => void saveSong(false)}>Save</button>
          <button onClick={() => void saveSong(true)}>Save As</button>
          <button className="primary" onClick={() => void playPreview()}>
            Play
          </button>
          <button onClick={() => void stopPreview()}>Stop</button>
        </div>
      </header>

      <div className="status-strip">
        <span>{dirty ? 'Unsaved changes' : 'Saved'}</span>
        <span>{currentPath ?? 'Untitled song'}</span>
        <span>{playback.running ? 'Engine preview running' : 'Engine preview idle'}</span>
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
                  <h2>Melody editor</h2>
                  <span>{totalBeats} beats visible</span>
                </div>
                <MelodyGrid
                  beatsPerBar={selectedSection.beatsPerBar ?? document.song.beatsPerBar}
                  totalBeats={totalBeats}
                  notes={selectedSection.melody}
                  selectedNoteId={selectedNoteId}
                  onSelectNote={setSelectedNoteId}
                  onUpsertNote={(note) => upsertNote(selectedSection.name, note)}
                />
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
            <CodeMirror value={yamlPreview} height="360px" extensions={[yamlLanguage()]} editable={false} />
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
