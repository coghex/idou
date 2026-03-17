import { type PointerEvent as ReactPointerEvent, type ReactNode, useCallback, useEffect, useMemo, useRef, useState } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { yaml as yamlLanguage } from '@codemirror/lang-yaml';
import { invoke, isTauri } from '@tauri-apps/api/core';
import { Menu } from '@tauri-apps/api/menu';
import MelodyGrid, { type OverlayNote } from './components/MelodyGrid';
import PatchGraphCanvas from './components/PatchGraphCanvas';
import { clamp, midiToNoteName, noteNameToMidi, quantizeBeat } from './lib/note';
import {
  clonePatchDocument,
  createDefaultPatch,
  createPatchConnection,
  createPatchNode,
  createSuggestedPatchFileName,
  nextPatchConnectionId,
  nextPatchNodeId,
  PATCH_ROLE_OPTIONS,
  parsePatchText,
  renamePatchNode,
  serializePatch,
  validatePatch,
  type PatchConnection,
  type PatchConnectionKind,
  type PatchDocument,
  type PatchNode,
  type PatchNodeType,
  type PatchRole,
} from './lib/patch';
import {
  analyzeChordRoman,
  buildSectionLoopDocument,
  inferChordToolContext,
  normalizeSectionName,
  parseSongText,
  createChordRegionId,
  sectionChordRegions,
  sectionChordRegionSpans,
  sectionPhraseBeats,
  sectionTotalBeats,
  serializeSong,
  syncSectionChordRegions,
  validateSong,
} from './lib/song';
import type { ChordRegion, MelodyNote, SongSection } from './lib/song';
import { useStudioStore } from './store/studioStore';

type FilePayload = {
  path: string;
  contents: string;
};

type SaveSongResponse = {
  path: string;
};

type PatchFileEntry = {
  path: string;
  label: string;
  source: string;
  assignPath: string;
};

type ValidatePatchResponse = {
  ok: boolean;
  message: string;
};

type PatchGraphToolMode = 'select' | 'wire';
type PatchWorkspaceTab = 'assign' | 'nodes' | 'connections';
type UtilityWorkspaceTab = 'validation' | 'yaml' | 'library' | 'engine';

type PanelId =
  | 'song'
  | 'sound'
  | 'sections'
  | 'form'
  | 'arrangementPreview'
  | 'patch'
  | 'patchNodes'
  | 'patchConnections'
  | 'yaml'
  | 'validation'
  | 'library'
  | 'engine';

type SectionDropHint = {
  sectionName: string;
  position: 'before' | 'after';
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

type SongMetadataOptions = {
  genres: string[];
  moods: string[];
};

type NoteClipboardPayload = Pick<MelodyNote, 'beat' | 'note' | 'duration' | 'velocity'>;

type InstrumentRoleMeta = {
  label: string;
  description: string;
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

const BEAT_UNIT_OPTIONS = [2, 4, 8, 16] as const;
const MIN_BEAT_WIDTH = 24;
const MAX_BEAT_WIDTH = 200;
const NOTE_CLIPBOARD_PREFIX = 'IDOU_NOTE:';

const INSTRUMENT_COLORS: Record<string, string> = {
  drums: 'rgba(229, 229, 229, 0.24)',
  bass: 'rgba(186, 186, 186, 0.24)',
  pad: 'rgba(148, 148, 148, 0.22)',
  arp: 'rgba(120, 120, 120, 0.22)',
  lead: 'rgba(245, 245, 245, 0.2)',
  'auto-drums': 'rgba(229, 229, 229, 0.24)',
};

const INSTRUMENT_ACCENTS: Record<string, string> = {
  drums: 'rgba(255, 255, 255, 0.82)',
  bass: 'rgba(236, 236, 236, 0.78)',
  pad: 'rgba(220, 220, 220, 0.72)',
  arp: 'rgba(205, 205, 205, 0.68)',
  lead: 'rgba(255, 255, 255, 0.88)',
  'auto-drums': 'rgba(255, 255, 255, 0.82)',
};

const PATCH_NODE_TYPE_OPTIONS: { label: string; value: PatchNodeType }[] = [
  { label: 'Oscillator', value: 'oscillator' },
  { label: 'Noise', value: 'noise' },
  { label: 'Envelope', value: 'envelope' },
  { label: 'Filter', value: 'filter' },
  { label: 'Output', value: 'output' },
];

const PATCH_CONNECTION_KIND_OPTIONS: { label: string; value: PatchConnectionKind }[] = [
  { label: 'Audio', value: 'signal' },
  { label: 'Volume shape', value: 'amp_envelope' },
  { label: 'Tone shape', value: 'filter_envelope' },
  { label: 'Hard sync', value: 'hard_sync' },
];

const PATCH_ROLE_META: Record<PatchRole, InstrumentRoleMeta> = {
  drums: {
    label: 'Drums',
    description: 'Percussion and groove.',
  },
  bass: {
    label: 'Bass',
    description: 'Low notes that support the harmony.',
  },
  pad: {
    label: 'Pad',
    description: 'Long background harmony and texture.',
  },
  arp: {
    label: 'Arpeggio',
    description: 'Repeating rhythmic pattern.',
  },
  lead: {
    label: 'Lead',
    description: 'Main melodic sound.',
  },
};

const DEFAULT_EXPANDED_PANELS: Record<PanelId, boolean> = {
  song: true,
  sound: true,
  sections: true,
  form: false,
  arrangementPreview: false,
  patch: true,
  patchNodes: true,
  patchConnections: false,
  yaml: false,
  validation: true,
  library: false,
  engine: false,
};

function isEditableTarget(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) {
    return false;
  }
  return Boolean(target.closest('input, textarea, select, button, .cm-editor')) || target.isContentEditable;
}

function isTextEntryTarget(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) {
    return false;
  }
  return Boolean(target.closest('input, textarea, select, .cm-editor')) || target.isContentEditable;
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

function ensureOptionValue(options: string[], value: string): string[] {
  const trimmed = value.trim();
  if (!trimmed || options.includes(trimmed)) {
    return options;
  }
  return [trimmed, ...options];
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

function countLabel(count: number, noun: string): string {
  return `${count} ${noun}${count === 1 ? '' : 's'}`;
}

type CollapsibleSectionProps = {
  title: string;
  expanded: boolean;
  onToggle: () => void;
  children: ReactNode;
  actions?: ReactNode;
  summary?: string;
  headingLevel?: 2 | 3;
  className?: string;
  bodyClassName?: string;
};

function CollapsibleSection({
  title,
  expanded,
  onToggle,
  children,
  actions,
  summary,
  headingLevel = 2,
  className,
  bodyClassName,
}: CollapsibleSectionProps) {
  const HeadingTag = headingLevel === 2 ? 'h2' : 'h3';
  const sectionClassName = ['collapsible-section', expanded ? 'expanded' : 'collapsed', className]
    .filter(Boolean)
    .join(' ');
  const contentClassName = ['panel-body', bodyClassName].filter(Boolean).join(' ');

  return (
    <section className={sectionClassName}>
      <div className="panel-header collapsible-header">
        <div className="panel-heading">
          <HeadingTag>{title}</HeadingTag>
          {summary ? <span className="panel-summary">{summary}</span> : null}
        </div>
        <div className="inline-actions">
          {actions}
          <button
            className="panel-toggle"
            onClick={onToggle}
            aria-expanded={expanded}
            title={expanded ? `Hide ${title}` : `Show ${title}`}
          >
            {expanded ? 'Hide' : 'Show'}
          </button>
        </div>
      </div>
      {expanded ? <div className={contentClassName}>{children}</div> : null}
    </section>
  );
}

function App() {
  const document = useStudioStore((state) => state.document);
  const currentPath = useStudioStore((state) => state.currentPath);
  const dirty = useStudioStore((state) => state.dirty);
  const selectedSectionName = useStudioStore((state) => state.selectedSectionName);
  const selectedNoteId = useStudioStore((state) => state.selectedNoteId);
  const editorMode = useStudioStore((state) => state.editorMode);
  const activePatchRole = useStudioStore((state) => state.activePatchRole);
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
  const reorderSection = useStudioStore((state) => state.reorderSection);
  const selectSection = useStudioStore((state) => state.selectSection);
  const enterPatchEditor = useStudioStore((state) => state.enterPatchEditor);
  const returnToArrangement = useStudioStore((state) => state.returnToArrangement);
  const setSelectedNoteId = useStudioStore((state) => state.setSelectedNoteId);
  const upsertNote = useStudioStore((state) => state.upsertNote);
  const removeNote = useStudioStore((state) => state.removeNote);
  const undo = useStudioStore((state) => state.undo);
  const redo = useStudioStore((state) => state.redo);
  const setPlayback = useStudioStore((state) => state.setPlayback);

  const [timelinePreview, setTimelinePreview] = useState<TimelinePreview>({ sections: [] });
  const [accompanimentLoading, setAccompanimentLoading] = useState(false);
  const [accompanimentDirty, setAccompanimentDirty] = useState(true);
  const [accompanimentError, setAccompanimentError] = useState<string | null>(null);
  const [snapStep, setSnapStep] = useState<number>(0.25);
  const [beatWidth, setBeatWidth] = useState<number>(42);
  const [showSectionDetails, setShowSectionDetails] = useState(false);
  const [showProgressionEditor, setShowProgressionEditor] = useState(false);
  const [showAccompaniment, setShowAccompaniment] = useState(true);
  const [auditionInstrument, setAuditionInstrument] = useState<string>('lead');
  const [selectedChordRegionId, setSelectedChordRegionId] = useState<string | null>(null);
  const [patchDocument, setPatchDocument] = useState<PatchDocument>(createDefaultPatch());
  const [patchPath, setPatchPath] = useState<string | null>(null);
  const [patchDirty, setPatchDirty] = useState(false);
  const [patchLibrary, setPatchLibrary] = useState<PatchFileEntry[]>([]);
  const [selectedPatchNodeId, setSelectedPatchNodeId] = useState<string>('osc1');
  const [selectedPatchConnectionId, setSelectedPatchConnectionId] = useState<string>('osc1_to_out');
  const [newPatchNodeType, setNewPatchNodeType] = useState<PatchNodeType>('oscillator');
  const [newPatchConnectionKind, setNewPatchConnectionKind] = useState<PatchConnectionKind>('signal');
  const [patchValidationMessage, setPatchValidationMessage] = useState<string>('Sound checks idle.');
  const [patchValidationOk, setPatchValidationOk] = useState<boolean | null>(null);
  const [activeYamlView, setActiveYamlView] = useState<'song' | 'patch'>('song');
  const [patchGraphToolMode, setPatchGraphToolMode] = useState<PatchGraphToolMode>('select');
  const [patchWorkspaceTab, setPatchWorkspaceTab] = useState<PatchWorkspaceTab>('assign');
  const [utilityWorkspaceTab, setUtilityWorkspaceTab] = useState<UtilityWorkspaceTab>('validation');
  const [expandedPanels, setExpandedPanels] = useState<Record<PanelId, boolean>>(DEFAULT_EXPANDED_PANELS);
  const [songMetadataOptions, setSongMetadataOptions] = useState<SongMetadataOptions>({ genres: [], moods: [] });
  const [draggedSectionName, setDraggedSectionName] = useState<string | null>(null);
  const [sectionDropHint, setSectionDropHint] = useState<SectionDropHint | null>(null);
  const sectionDropHintRef = useRef<SectionDropHint | null>(null);
  const sectionPointerDragRef = useRef<{ sectionName: string; startX: number; startY: number; active: boolean } | null>(null);
  const menuActionRef = useRef({
    newDocument: () => {},
    openSong: () => {},
    saveSong: (_forceDialog = false) => {},
    undo: () => {},
    redo: () => {},
  });

  const selectedSection =
    document.sections.find((section) => section.name === selectedSectionName) ?? document.sections[0] ?? null;
  const selectedNote = selectedSection?.melody.find((note) => note.id === selectedNoteId) ?? null;
  const yamlPreview = useMemo(() => serializeSong(document), [document]);
  const patchYamlPreview = useMemo(() => serializePatch(patchDocument), [patchDocument]);
  const validationIssues = useMemo(() => validateSong(document), [document]);
  const patchIssues = useMemo(() => validatePatch(patchDocument), [patchDocument]);
  const selectedTimelineSection =
    timelinePreview.sections.find((section) => section.name === selectedSection?.name) ?? null;
  const totalBeats = selectedSection ? sectionTotalBeats(document, selectedSection) : 0;
  const activeBeatsPerBar = selectedSection?.beatsPerBar ?? document.song.beatsPerBar;
  const phraseBeats = selectedSection ? sectionPhraseBeats(document, selectedSection) : 0;
  const totalBars = Math.max(1, Math.ceil(totalBeats / activeBeatsPerBar));
  const currentPatchLabel = useMemo(() => basename(patchPath), [patchPath]);
  const activePatchRoleMeta = activePatchRole ? PATCH_ROLE_META[activePatchRole] : null;
  const currentFileDisplay = currentPath ?? 'Untitled project';
  const fileStatusTone = currentPath ? (dirty ? 'dirty' : 'saved') : 'new';
  const fileStatusLabel = currentPath ? (dirty ? 'Unsaved' : 'Saved') : 'Untitled';
  const songGenreOptions = useMemo(
    () => ensureOptionValue(songMetadataOptions.genres, document.song.genre),
    [document.song.genre, songMetadataOptions.genres],
  );
  const songMoodOptions = useMemo(
    () => ensureOptionValue(songMetadataOptions.moods, document.song.mood),
    [document.song.mood, songMetadataOptions.moods],
  );
  const selectedSectionChordRegions = useMemo(
    () => (selectedSection ? sectionChordRegions(document, selectedSection) : []),
    [document, selectedSection],
  );
  const selectedSectionChordRegionSpans = useMemo(
    () => (selectedSection ? sectionChordRegionSpans(document, selectedSection) : []),
    [document, selectedSection],
  );
  const chordContext = useMemo(
    () => inferChordToolContext(selectedSectionChordRegions.map((region) => region.symbol)),
    [selectedSectionChordRegions],
  );
  const chordRomans = useMemo(
    () => selectedSectionChordRegions.map((region) => analyzeChordRoman(region.symbol, chordContext.keyRoot, chordContext.mode)),
    [chordContext.keyRoot, chordContext.mode, selectedSectionChordRegions],
  );
  const selectedChordRegion =
    selectedSectionChordRegions.find((region) => region.id === selectedChordRegionId) ?? selectedSectionChordRegions[0] ?? null;
  const selectedChordRegionSpan =
    selectedSectionChordRegionSpans.find((region) => region.id === selectedChordRegion?.id) ?? selectedSectionChordRegionSpans[0] ?? null;
  const selectedChordRegionRoman =
    selectedChordRegion && selectedSectionChordRegions.length > 0
      ? chordRomans[selectedSectionChordRegions.findIndex((region) => region.id === selectedChordRegion.id)] ?? selectedChordRegion.symbol
      : null;
  const patchNodeEntries = useMemo(() => Object.entries(patchDocument.patch.nodes), [patchDocument]);
  const patchConnectionEntries = useMemo(() => Object.entries(patchDocument.patch.connections), [patchDocument]);
  const selectedPatchNode = selectedPatchNodeId ? patchDocument.patch.nodes[selectedPatchNodeId] ?? null : null;
  const selectedPatchConnection =
    selectedPatchConnectionId ? patchDocument.patch.connections[selectedPatchConnectionId] ?? null : null;
  const patchSelectOptions = useMemo(() => {
    const byAssignPath = new Map(patchLibrary.map((entry) => [entry.assignPath, entry]));
    for (const role of PATCH_ROLE_OPTIONS) {
      const assigned = document.song.patches[role];
      if (assigned && !byAssignPath.has(assigned)) {
        byAssignPath.set(assigned, {
          path: assigned,
          assignPath: assigned,
          label: assigned,
          source: 'custom',
        });
      }
    }
    return [...byAssignPath.values()].sort((left, right) => left.label.localeCompare(right.label));
  }, [document.song.patches, patchLibrary]);
  const patchOverrideCount = useMemo(
    () => PATCH_ROLE_OPTIONS.filter((role) => Boolean(document.song.patches[role])).length,
    [document.song.patches],
  );
  const currentPatchAssignPath = useMemo(
    () => (patchPath ? patchLibrary.find((entry) => entry.path === patchPath)?.assignPath ?? patchPath : null),
    [patchLibrary, patchPath],
  );
  const currentPatchAssignedRoles = useMemo(
    () =>
      currentPatchAssignPath
        ? PATCH_ROLE_OPTIONS.filter((role) => document.song.patches[role] === currentPatchAssignPath)
        : [],
    [currentPatchAssignPath, document.song.patches],
  );
  const totalValidationIssues = validationIssues.length + patchIssues.length + (patchValidationOk === false ? 1 : 0);
  const validationSummary =
    totalValidationIssues === 0 ? 'Checks look good' : `${countLabel(totalValidationIssues, 'issue')} to review`;
  const accompanimentSummary = accompanimentLoading
    ? 'Updating preview'
    : accompanimentDirty
      ? 'Preview needs update'
      : selectedTimelineSection
        ? `${countLabel(selectedTimelineSection.notes.length, 'generated backing note')}`
        : 'Preview not ready';
  const utilityWorkspaceSummary =
    utilityWorkspaceTab === 'validation'
      ? validationSummary
      : utilityWorkspaceTab === 'yaml'
        ? activeYamlView === 'song'
          ? 'Song file preview'
          : 'Sound file preview'
        : utilityWorkspaceTab === 'library'
          ? countLabel(patchLibrary.length, 'sound file')
          : playback.logLines.length === 0
            ? 'Playback log idle'
            : transportLabel(playback);

  const togglePanel = useCallback((panelId: PanelId) => {
    setExpandedPanels((current) => ({
      ...current,
      [panelId]: !current[panelId],
    }));
  }, []);

  const setActiveSectionDropHint = useCallback((nextHint: SectionDropHint | null) => {
    sectionDropHintRef.current = nextHint;
    setSectionDropHint((current) => {
      if (
        current?.sectionName === nextHint?.sectionName &&
        current?.position === nextHint?.position
      ) {
        return current;
      }
      return nextHint;
    });
  }, []);

  const clearSectionDragState = useCallback(() => {
    sectionPointerDragRef.current = null;
    sectionDropHintRef.current = null;
    setDraggedSectionName(null);
    setSectionDropHint(null);
    window.document.body.classList.remove('section-reordering');
  }, []);

  const commitSectionDrop = useCallback(
    (sourceSectionName: string, hint: SectionDropHint | null) => {
      if (!hint || sourceSectionName === hint.sectionName) {
        return;
      }

      const sourceIndex = document.sections.findIndex((section) => section.name === sourceSectionName);
      const targetIndex = document.sections.findIndex((section) => section.name === hint.sectionName);
      if (sourceIndex < 0 || targetIndex < 0) {
        return;
      }

      let nextIndex = targetIndex + (hint.position === 'after' ? 1 : 0);
      if (sourceIndex < nextIndex) {
        nextIndex -= 1;
      }
      reorderSection(sourceSectionName, nextIndex);
    },
    [document.sections, reorderSection],
  );

  const updateSectionDropHintFromPoint = useCallback(
    (clientX: number, clientY: number, sourceSectionName: string) => {
      const element = window.document.elementFromPoint(clientX, clientY)?.closest<HTMLElement>('[data-section-drop-target]');
      const targetSectionName = element?.dataset.sectionDropTarget ?? null;
      if (!element || !targetSectionName || targetSectionName === sourceSectionName) {
        setActiveSectionDropHint(null);
        return;
      }

      const axis = element.dataset.sectionDropAxis === 'vertical' ? 'vertical' : 'horizontal';
      const bounds = element.getBoundingClientRect();
      const position =
        axis === 'vertical'
          ? clientY < bounds.top + bounds.height / 2
            ? 'before'
            : 'after'
          : clientX < bounds.left + bounds.width / 2
            ? 'before'
            : 'after';

      setActiveSectionDropHint({ sectionName: targetSectionName, position });
    },
    [setActiveSectionDropHint],
  );

  const beginSectionPointerDrag = useCallback((event: ReactPointerEvent<HTMLElement>, sectionName: string) => {
    event.preventDefault();
    event.stopPropagation();
    sectionPointerDragRef.current = {
      sectionName,
      startX: event.clientX,
      startY: event.clientY,
      active: false,
    };
    setActiveSectionDropHint(null);
  }, []);

  useEffect(() => {
    const handlePointerMove = (event: PointerEvent) => {
      const drag = sectionPointerDragRef.current;
      if (!drag) {
        return;
      }

      if (!drag.active) {
        const movedEnough = Math.abs(event.clientX - drag.startX) > 4 || Math.abs(event.clientY - drag.startY) > 4;
        if (!movedEnough) {
          return;
        }

        drag.active = true;
        setDraggedSectionName(drag.sectionName);
        window.document.body.classList.add('section-reordering');
      }

      updateSectionDropHintFromPoint(event.clientX, event.clientY, drag.sectionName);
      event.preventDefault();
    };

    const handlePointerUp = (event: PointerEvent) => {
      const drag = sectionPointerDragRef.current;
      if (!drag) {
        return;
      }

      if (drag.active) {
        updateSectionDropHintFromPoint(event.clientX, event.clientY, drag.sectionName);
        commitSectionDrop(drag.sectionName, sectionDropHintRef.current);
      }

      clearSectionDragState();
    };

    window.addEventListener('pointermove', handlePointerMove, { passive: false });
    window.addEventListener('pointerup', handlePointerUp);
    return () => {
      window.removeEventListener('pointermove', handlePointerMove);
      window.removeEventListener('pointerup', handlePointerUp);
    };
  }, [clearSectionDragState, commitSectionDrop, updateSectionDropHintFromPoint]);

  useEffect(() => {
    let cancelled = false;

    async function loadSongMetadataOptions() {
      try {
        const options = await invoke<SongMetadataOptions>('song_metadata_options');
        if (!cancelled) {
          setSongMetadataOptions(options);
        }
      } catch (error) {
        if (!cancelled) {
          setStatusMessage(`Failed to load song options: ${error instanceof Error ? error.message : String(error)}`);
        }
      }
    }

    void loadSongMetadataOptions();

    return () => {
      cancelled = true;
    };
  }, [setStatusMessage]);

  useEffect(() => {
    if (selectedSectionChordRegions.length === 0) {
      setSelectedChordRegionId(null);
      return;
    }
    if (!selectedChordRegionId || !selectedSectionChordRegions.some((region) => region.id === selectedChordRegionId)) {
      setSelectedChordRegionId(selectedSectionChordRegions[0]?.id ?? null);
    }
  }, [selectedChordRegionId, selectedSectionChordRegions]);

  useEffect(() => {
    setAccompanimentDirty(true);
  }, [yamlPreview]);

  useEffect(() => {
    if (!patchDocument.patch.nodes[selectedPatchNodeId]) {
      setSelectedPatchNodeId(Object.keys(patchDocument.patch.nodes)[0] ?? '');
    }
  }, [patchDocument, selectedPatchNodeId]);

  useEffect(() => {
    if (!patchDocument.patch.connections[selectedPatchConnectionId]) {
      setSelectedPatchConnectionId(Object.keys(patchDocument.patch.connections)[0] ?? '');
    }
  }, [patchDocument, selectedPatchConnectionId]);

  useEffect(() => {
    const loadPatchLibrary = async () => {
      try {
        const files = await invoke<PatchFileEntry[]>('list_patch_files', {
          request: {
            songPath: currentPath,
            currentPatchPath: patchPath,
          },
        });
        setPatchLibrary(files);
      } catch (error) {
        console.error(error);
      }
    };

    void loadPatchLibrary();
  }, [currentPath, patchPath]);

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

  useEffect(() => {
    menuActionRef.current = {
      newDocument: () => newDocument(),
      openSong: () => {
        void openSong();
      },
      saveSong: (forceDialog = false) => {
        void saveSong(forceDialog);
      },
      undo: () => undo(),
      redo: () => redo(),
    };
  }, [newDocument, openSong, redo, saveSong, undo]);

  useEffect(() => {
    if (!isTauri()) {
      return;
    }

    let disposed = false;
    let appMenu: Menu | null = null;

    async function installAppMenu() {
      const menu = await Menu.new({
        items: [
          {
            text: 'File',
            items: [
              {
                id: 'file.new',
                text: 'New',
                accelerator: 'CmdOrCtrl+N',
                action: () => menuActionRef.current.newDocument(),
              },
              {
                id: 'file.open',
                text: 'Open…',
                accelerator: 'CmdOrCtrl+O',
                action: () => menuActionRef.current.openSong(),
              },
              {
                id: 'file.save',
                text: 'Save',
                accelerator: 'CmdOrCtrl+S',
                action: () => menuActionRef.current.saveSong(false),
              },
              {
                id: 'file.saveAs',
                text: 'Save As…',
                accelerator: 'CmdOrCtrl+Shift+S',
                action: () => menuActionRef.current.saveSong(true),
              },
              { item: 'Separator' },
              { item: 'Quit' },
            ],
          },
          {
            text: 'Edit',
            items: [
              {
                id: 'edit.undo',
                text: 'Undo',
                accelerator: 'CmdOrCtrl+Z',
                action: () => menuActionRef.current.undo(),
              },
              {
                id: 'edit.redo',
                text: 'Redo',
                accelerator: 'CmdOrCtrl+Shift+Z',
                action: () => menuActionRef.current.redo(),
              },
            ],
          },
        ],
      });

      if (disposed) {
        await menu.close();
        return;
      }

      await menu.setAsAppMenu();
      appMenu = menu;
    }

    void installAppMenu().catch((error) => {
      if (!disposed) {
        setStatusMessage(`Failed to install app menu: ${error instanceof Error ? error.message : String(error)}`);
      }
    });

    return () => {
      disposed = true;
      if (appMenu) {
        void appMenu.close();
      }
    };
  }, [setStatusMessage]);

  const loadPatchDocument = useCallback(
    (documentToLoad: PatchDocument, nextPath: string | null, message: string) => {
      setPatchDocument(documentToLoad);
      setPatchPath(nextPath);
      setPatchDirty(false);
      setPatchValidationMessage('Sound checks idle.');
      setPatchValidationOk(null);
      setSelectedPatchNodeId(Object.keys(documentToLoad.patch.nodes)[0] ?? '');
      setSelectedPatchConnectionId(Object.keys(documentToLoad.patch.connections)[0] ?? '');
      setActiveYamlView('patch');
      setStatusMessage(message);
    },
    [setStatusMessage],
  );

  const newPatch = useCallback(() => {
    loadPatchDocument(createDefaultPatch(), null, 'Created a new sound.');
  }, [loadPatchDocument]);

  const openPatch = useCallback(async () => {
    try {
      const payload = await invoke<FilePayload | null>('open_patch_file');
      if (!payload) {
        return;
      }
      const parsed = parsePatchText(payload.contents);
      loadPatchDocument(parsed, payload.path, `Opened sound ${payload.path}`);
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [loadPatchDocument, setStatusMessage]);

  const openPatchFromLibrary = useCallback(
    async (path: string) => {
      try {
        const payload = await invoke<FilePayload>('open_patch_file_at_path', {
          request: {
            path,
          },
        });
        const parsed = parsePatchText(payload.contents);
        loadPatchDocument(parsed, payload.path, `Opened sound ${payload.path}`);
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [loadPatchDocument, setStatusMessage],
  );

  const openPatchEditorForRole = useCallback(
    async (role: PatchRole) => {
      const assignedPath = document.song.patches[role];
      const roleMeta = PATCH_ROLE_META[role];
      enterPatchEditor(role);
      setPatchWorkspaceTab('assign');
      setUtilityWorkspaceTab('validation');
      setActiveYamlView('patch');

      if (!assignedPath) {
        const starter = createDefaultPatch();
        starter.patch.name = `${roleMeta.label} Sound`;
        loadPatchDocument(starter, null, `Started a new sound for ${roleMeta.label}.`);
        return;
      }

      try {
        const payload = await invoke<FilePayload>('open_patch_file_at_path', {
          request: {
            path: assignedPath,
          },
        });
        const parsed = parsePatchText(payload.contents);
        loadPatchDocument(parsed, payload.path, `Editing the ${roleMeta.label} sound.`);
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [document.song.patches, enterPatchEditor, loadPatchDocument, setStatusMessage],
  );

  const savePatch = useCallback(
    async (forceDialog = false) => {
      try {
        const response = await invoke<SaveSongResponse | null>('save_patch_file', {
          request: {
            path: forceDialog ? null : patchPath,
            contents: patchYamlPreview,
            suggestedName: createSuggestedPatchFileName(patchDocument.patch.name),
          },
        });
        if (!response) {
          return;
        }
        setPatchPath(response.path);
        setPatchDirty(false);
        setPatchValidationMessage(`Saved ${response.path}`);
        setPatchValidationOk(true);
        setStatusMessage(`Saved sound ${response.path}`);
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
      }
    },
    [patchDocument.patch.name, patchPath, patchYamlPreview, setStatusMessage],
  );

  const validateCurrentPatch = useCallback(async () => {
    try {
      const response = await invoke<ValidatePatchResponse>('validate_patch_contents', {
        request: {
          contents: patchYamlPreview,
          suggestedName: createSuggestedPatchFileName(patchDocument.patch.name),
        },
      });
      setPatchValidationMessage(response.message);
      setPatchValidationOk(response.ok);
      setStatusMessage(response.message);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      setPatchValidationMessage(message);
      setPatchValidationOk(false);
      setStatusMessage(message);
    }
  }, [patchDocument.patch.name, patchYamlPreview, setStatusMessage]);

  const auditionCurrentPatch = useCallback(async () => {
    if (!patchPath && patchDirty) {
      setStatusMessage('Save this sound before previewing it through the engine.');
      return;
    }
    if (!patchPath) {
      setStatusMessage('Open or save a sound file before previewing.');
      return;
    }
    try {
      await invoke('preview_patch_note', {
        request: {
          path: patchPath,
          midi: selectedNote?.note ?? 60,
          velocity: selectedNote?.velocity ?? 0.72,
          durationMs: 520,
        },
      });
      setStatusMessage(`Previewed ${currentPatchLabel}.`);
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
    }
  }, [currentPatchLabel, patchDirty, patchPath, selectedNote, setStatusMessage]);

  function updatePatchDocument(updater: (document: PatchDocument) => PatchDocument) {
    setPatchDocument((current) => updater(current));
    setPatchDirty(true);
    setPatchValidationOk(null);
    setPatchValidationMessage('Sound changed. Run checks when ready.');
  }

  function updatePatchNode(nodeId: string, updater: (node: PatchNode) => PatchNode) {
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      const node = next.patch.nodes[nodeId];
      if (!node) {
        return current;
      }
      next.patch.nodes[nodeId] = updater(node);
      return next;
    });
  }

  function updatePatchConnection(connectionId: string, updater: (connection: PatchConnection) => PatchConnection) {
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      const connection = next.patch.connections[connectionId];
      if (!connection) {
        return current;
      }
      next.patch.connections[connectionId] = updater(connection);
      return next;
    });
  }

  function setSongPatch(role: PatchRole, path: string | null) {
    setSongField('patches', {
      ...document.song.patches,
      [role]: path,
    });
  }

  function assignCurrentPatchToRole(role: PatchRole) {
    if (!currentPatchAssignPath) {
      setStatusMessage('Save this sound before assigning it to an instrument.');
      return;
    }
    setSongPatch(role, currentPatchAssignPath);
    setStatusMessage(`Assigned ${currentPatchLabel} to ${PATCH_ROLE_META[role].label}.`);
  }

  function addPatchNodeOfType(nodeType: PatchNodeType) {
    const prefix =
      nodeType === 'oscillator'
        ? 'osc'
        : nodeType === 'envelope'
          ? 'env'
          : nodeType === 'filter'
            ? 'filter'
            : nodeType === 'noise'
              ? 'noise'
              : 'out';
    const nextId = nextPatchNodeId(patchDocument, prefix);
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      next.patch.nodes[nextId] = createPatchNode(nodeType);
      return next;
    });
    setSelectedPatchNodeId(nextId);
    setStatusMessage(`Added ${nodeType} node ${nextId}.`);
  }

  function addPatchNode() {
    addPatchNodeOfType(newPatchNodeType);
  }

  function removePatchNode(nodeId: string) {
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      delete next.patch.nodes[nodeId];
      next.patch.connections = Object.fromEntries(
        Object.entries(next.patch.connections).filter(
          ([, connection]) => connection.from !== nodeId && connection.to !== nodeId,
        ),
      );
      return next;
    });
    setStatusMessage(`Removed node ${nodeId}.`);
  }

  function renameSelectedPatchNode(nextId: string) {
    if (!selectedPatchNodeId) {
      return;
    }
    const renamed = renamePatchNode(patchDocument, selectedPatchNodeId, nextId);
    if (renamed !== patchDocument) {
      setPatchDocument(renamed);
      setPatchDirty(true);
      setPatchValidationOk(null);
      setPatchValidationMessage('Sound changed. Run checks when ready.');
      setSelectedPatchNodeId(nextId.trim());
    }
  }

  function addPatchConnection() {
    const nextId = nextPatchConnectionId(patchDocument);
    const defaultFrom = selectedPatchNodeId || Object.keys(patchDocument.patch.nodes)[0] || '';
    const defaultTo =
      Object.keys(patchDocument.patch.nodes).find((nodeId) => nodeId !== defaultFrom) ?? defaultFrom;
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      next.patch.connections[nextId] = createPatchConnection(defaultFrom, defaultTo, newPatchConnectionKind);
      return next;
    });
    setSelectedPatchConnectionId(nextId);
    setStatusMessage(`Added ${newPatchConnectionKind} connection ${nextId}.`);
  }

  function removePatchConnection(connectionId: string) {
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      delete next.patch.connections[connectionId];
      return next;
    });
    setStatusMessage(`Removed connection ${connectionId}.`);
  }

  function connectPatchNodes(fromNodeId: string, toNodeId: string, kind: PatchConnectionKind) {
    const prefix = `${fromNodeId}-to-${toNodeId}`;
    const nextId = nextPatchConnectionId(patchDocument, prefix);
    updatePatchDocument((current) => {
      const next = clonePatchDocument(current);
      next.patch.connections[nextId] = createPatchConnection(fromNodeId, toNodeId, kind);
      return next;
    });
    setSelectedPatchConnectionId(nextId);
    setStatusMessage(`Wired ${fromNodeId} → ${toNodeId} as ${kind}.`);
  }

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

  const serializeNoteClipboard = useCallback((payload: NoteClipboardPayload) => {
    return `${NOTE_CLIPBOARD_PREFIX}${JSON.stringify(payload)}`;
  }, []);

  const parseNoteClipboard = useCallback((rawText: string): NoteClipboardPayload | null => {
    if (!rawText.startsWith(NOTE_CLIPBOARD_PREFIX)) {
      return null;
    }

    try {
      const parsed = JSON.parse(rawText.slice(NOTE_CLIPBOARD_PREFIX.length)) as Partial<NoteClipboardPayload>;
      if (
        typeof parsed.beat !== 'number' ||
        typeof parsed.note !== 'number' ||
        typeof parsed.duration !== 'number' ||
        typeof parsed.velocity !== 'number'
      ) {
        return null;
      }
      return parsed as NoteClipboardPayload;
    } catch {
      return null;
    }
  }, []);

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
    const handleCopy = (event: ClipboardEvent) => {
      if (editorMode !== 'arrangement' || !selectedNote || isTextEntryTarget(event.target)) {
        return;
      }

      const payload = serializeNoteClipboard({
        beat: selectedNote.beat,
        note: selectedNote.note,
        duration: selectedNote.duration,
        velocity: selectedNote.velocity,
      });

      event.preventDefault();
      event.clipboardData?.setData('text/plain', payload);
      setStatusMessage(`Copied ${midiToNoteName(selectedNote.note)}.`);
    };

    const handleCut = (event: ClipboardEvent) => {
      if (editorMode !== 'arrangement' || !selectedSection || !selectedNote || isTextEntryTarget(event.target)) {
        return;
      }

      const payload = serializeNoteClipboard({
        beat: selectedNote.beat,
        note: selectedNote.note,
        duration: selectedNote.duration,
        velocity: selectedNote.velocity,
      });

      event.preventDefault();
      event.clipboardData?.setData('text/plain', payload);
      removeNote(selectedSection.name, selectedNote.id);
      setStatusMessage(`Cut ${midiToNoteName(selectedNote.note)}.`);
    };

    const handlePaste = (event: ClipboardEvent) => {
      if (editorMode !== 'arrangement' || !selectedSection || isTextEntryTarget(event.target)) {
        return;
      }

      const rawText = event.clipboardData?.getData('text/plain') ?? '';
      const payload = parseNoteClipboard(rawText);
      if (!payload) {
        return;
      }

      event.preventDefault();
      const duration = Math.max(snapStep, Math.min(payload.duration, totalBeats));
      const anchorBeat = selectedNote ? selectedNote.beat + selectedNote.duration : payload.beat;
      const beat = clamp(
        quantizeBeat(anchorBeat, snapStep),
        0,
        Math.max(0, totalBeats - duration),
      );
      const pastedNote: MelodyNote = {
        id: crypto.randomUUID(),
        beat,
        note: payload.note,
        duration,
        velocity: payload.velocity,
      };
      upsertNote(selectedSection.name, pastedNote);
      setSelectedNoteId(pastedNote.id);
      setStatusMessage(`Pasted ${midiToNoteName(payload.note)}.`);
    };

    window.addEventListener('copy', handleCopy);
    window.addEventListener('cut', handleCut);
    window.addEventListener('paste', handlePaste);
    return () => {
      window.removeEventListener('copy', handleCopy);
      window.removeEventListener('cut', handleCut);
      window.removeEventListener('paste', handlePaste);
    };
  }, [
    editorMode,
    parseNoteClipboard,
    removeNote,
    selectedNote,
    selectedSection,
    serializeNoteClipboard,
    setSelectedNoteId,
    setStatusMessage,
    snapStep,
    totalBeats,
    upsertNote,
  ]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      const modifier = event.metaKey || event.ctrlKey;

      if (modifier && event.code === 'KeyN') {
        event.preventDefault();
        newDocument();
        return;
      }

      if (modifier && event.code === 'KeyS') {
        event.preventDefault();
        void saveSong(event.shiftKey);
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

      if (event.code === 'Space') {
        if (event.repeat) {
          return;
        }
        event.preventDefault();
        void togglePlayback();
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
    newDocument,
    nudgeSelectedNote,
    openSong,
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

  function setSelectedSectionChordRegions(regions: ChordRegion[]) {
    if (!selectedSection) {
      return;
    }
    updateSelectedSection((section) => syncSectionChordRegions(document, section, regions));
  }

  function updateSelectedChordRegion(updater: (region: ChordRegion) => ChordRegion) {
    if (!selectedChordRegion) {
      return;
    }
    setSelectedSectionChordRegions(
      selectedSectionChordRegions.map((region) => (region.id === selectedChordRegion.id ? updater(region) : region)),
    );
  }

  function addChordChange() {
    if (!selectedSection) {
      return;
    }
    const activeSpan =
      selectedSectionChordRegionSpans.find((region) => region.id === selectedChordRegion?.id) ??
      selectedSectionChordRegionSpans[selectedSectionChordRegionSpans.length - 1] ??
      null;
    const defaultStart =
      activeSpan && activeSpan.durationBeats > snapStep
        ? quantizeBeat(activeSpan.startBeat + activeSpan.durationBeats / 2, snapStep)
        : quantizeBeat(Math.max(snapStep, phraseBeats - snapStep), snapStep);
    const newRegion = {
      id: createChordRegionId(),
      symbol: selectedChordRegion?.symbol ?? 'Am',
      startBeat: Math.min(Math.max(snapStep, defaultStart), Math.max(snapStep, phraseBeats - snapStep)),
    } satisfies ChordRegion;
    setSelectedSectionChordRegions([...selectedSectionChordRegions, newRegion]);
    setSelectedChordRegionId(newRegion.id);
    setStatusMessage(`Added chord change at beat ${newRegion.startBeat}.`);
  }

  function removeSelectedChordChange() {
    if (!selectedChordRegion || selectedSectionChordRegions.length <= 1) {
      return;
    }
    const currentIndex = selectedSectionChordRegions.findIndex((region) => region.id === selectedChordRegion.id);
    const nextRegions = selectedSectionChordRegions.filter((region) => region.id !== selectedChordRegion.id);
    setSelectedSectionChordRegions(nextRegions);
    setSelectedChordRegionId(nextRegions[Math.max(0, currentIndex - 1)]?.id ?? nextRegions[0]?.id ?? null);
    setStatusMessage('Removed chord change.');
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

  const auditionInstrumentLabel =
    AUDITION_INSTRUMENT_OPTIONS.find((option) => option.value === auditionInstrument)?.label ?? auditionInstrument;
  const arrangementPreviewToggleLabel = showAccompaniment ? 'Hide backing notes' : 'Show backing notes';
  const instrumentsSummary =
    patchOverrideCount === 0 ? 'Using built-in sounds' : `${countLabel(patchOverrideCount, 'custom sound')} active`;
  const soundSetupSummary =
    currentPatchAssignedRoles.length === 0
      ? `${currentPatchLabel} · not assigned`
      : `${currentPatchLabel} · ${countLabel(currentPatchAssignedRoles.length, 'role')} active`;

  const projectToolsSection = (
    <CollapsibleSection
      title={editorMode === 'patch' ? 'Utilities' : 'Project tools'}
      summary={utilityWorkspaceSummary}
      expanded={expandedPanels.validation}
      onToggle={() => togglePanel('validation')}
    >
      <div className="segmented-tabs">
        <button
          className={utilityWorkspaceTab === 'validation' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setUtilityWorkspaceTab('validation')}
          title="Show song and sound checks."
        >
          Checks
        </button>
        <button
          className={utilityWorkspaceTab === 'yaml' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setUtilityWorkspaceTab('yaml')}
          title="Preview the files that will be saved."
        >
          File preview
        </button>
        <button
          className={utilityWorkspaceTab === 'library' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setUtilityWorkspaceTab('library')}
          title="Browse available sound files."
        >
          Sound library
        </button>
        <button
          className={utilityWorkspaceTab === 'engine' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setUtilityWorkspaceTab('engine')}
          title="Inspect playback messages from the engine."
        >
          Playback log
        </button>
      </div>
      {utilityWorkspaceTab === 'validation' ? (
        <>
          {validationIssues.length === 0 ? (
            <div className="validation-ok">Song file looks good.</div>
          ) : (
            <ul className="validation-list">
              {validationIssues.map((issue) => (
                <li key={issue}>{issue}</li>
              ))}
            </ul>
          )}
          {patchIssues.length === 0 ? (
            <div className="validation-ok">Sound file looks good.</div>
          ) : (
            <ul className="validation-list">
              {patchIssues.map((issue) => (
                <li key={issue}>{issue}</li>
              ))}
            </ul>
          )}
          <div className={patchValidationOk === false ? 'validation-inline' : 'hint'}>{patchValidationMessage}</div>
        </>
      ) : null}
      {utilityWorkspaceTab === 'yaml' ? (
        <>
          <div className="segmented-tabs compact-tabs">
            <button
              className={activeYamlView === 'song' ? 'toggle-button active' : 'toggle-button'}
              onClick={() => setActiveYamlView('song')}
              title="Preview the current song file."
            >
              Song file
            </button>
            <button
              className={activeYamlView === 'patch' ? 'toggle-button active' : 'toggle-button'}
              onClick={() => setActiveYamlView('patch')}
              title="Preview the current sound file."
            >
              Sound file
            </button>
          </div>
          <CodeMirror
            value={activeYamlView === 'song' ? yamlPreview : patchYamlPreview}
            height="320px"
            extensions={[yamlLanguage()]}
            editable={false}
          />
        </>
      ) : null}
      {utilityWorkspaceTab === 'library' ? (
        <div className="patch-library-list">
          {patchLibrary.length === 0 ? (
            <div className="hint">No sound files found yet.</div>
          ) : (
            patchLibrary.map((entry) => (
              <button
                key={entry.path}
                className="patch-library-item"
                onClick={() => void openPatchFromLibrary(entry.path)}
                title={`Open ${entry.label} from the ${entry.source} sound library.`}
              >
                <span>{entry.label}</span>
                <span className="section-chip-meta">{entry.source}</span>
              </button>
            ))
          )}
        </div>
      ) : null}
      {utilityWorkspaceTab === 'engine' ? (
        <div className="log-panel">{playback.logLines.length === 0 ? 'No playback messages yet.' : playback.logLines.join('\n')}</div>
      ) : null}
    </CollapsibleSection>
  );

  const soundSetupSection = (
    <CollapsibleSection
      title="Sound setup"
      summary={soundSetupSummary}
      expanded={expandedPanels.patch}
      onToggle={() => togglePanel('patch')}
      actions={
        <>
          <button onClick={() => newPatch()} title="Start a fresh sound document.">
            New
          </button>
          <button onClick={() => void openPatch()} title="Open an existing sound file.">
            Open
          </button>
          <button onClick={() => void savePatch(false)} title="Save the current sound.">
            Save
          </button>
          <button onClick={() => void savePatch(true)} title="Save the current sound to a new file path.">
            Save As
          </button>
        </>
      }
    >
      <div className="patch-toolbar">
        <div className="file-pill">
          <span className={patchDirty ? 'dirty-indicator dirty' : 'dirty-indicator'} />
          <span>{currentPatchLabel}</span>
        </div>
        <div className="inline-actions">
          <button onClick={() => void validateCurrentPatch()} title="Check the current sound against the engine validator.">
            Check
          </button>
          <button onClick={() => void auditionCurrentPatch()} title="Play a short preview through the current sound.">
            Preview
          </button>
        </div>
      </div>
      <div className="patch-metrics">
        <span className="toolbar-badge">{countLabel(patchNodeEntries.length, 'block')}</span>
        <span className="toolbar-badge">{countLabel(patchConnectionEntries.length, 'connection')}</span>
        <span className="toolbar-badge">
          {currentPatchAssignedRoles.length === 0 ? 'Not assigned yet' : `Used by ${currentPatchAssignedRoles.join(', ')}`}
        </span>
      </div>
      <div className="segmented-tabs">
        <button
          className={patchWorkspaceTab === 'assign' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setPatchWorkspaceTab('assign')}
          title="Name this sound and decide which instruments should use it."
        >
          Assignments
        </button>
        <button
          className={patchWorkspaceTab === 'nodes' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setPatchWorkspaceTab('nodes')}
          title="Inspect the sound-building blocks."
        >
          Building blocks
        </button>
        <button
          className={patchWorkspaceTab === 'connections' ? 'toggle-button active' : 'toggle-button'}
          onClick={() => setPatchWorkspaceTab('connections')}
          title="Inspect how the blocks connect together."
        >
          Routing
        </button>
      </div>
      {patchWorkspaceTab === 'assign' ? (
        <>
          {activePatchRoleMeta ? (
            <div className="hint">
              You opened this sound from {activePatchRoleMeta.label}. Save it, then keep or change that assignment below.
            </div>
          ) : null}
          <div className="patch-role-grid patch-role-list-compact">
            {PATCH_ROLE_OPTIONS.map((role) => {
              const roleMeta = PATCH_ROLE_META[role];
              return (
                <div
                  key={role}
                  className={
                    role === activePatchRole || document.song.patches[role] === currentPatchAssignPath
                      ? 'patch-role-card active'
                      : 'patch-role-card'
                  }
                  title={`Choose which sound the ${roleMeta.label} part should use.`}
                >
                  <div className="patch-role-card-head">
                    <strong>{roleMeta.label}</strong>
                    <span className="section-chip-meta">{document.song.patches[role] ? 'custom' : 'built-in'}</span>
                  </div>
                  <span className="hint">
                    {document.song.patches[role] ? basename(document.song.patches[role]) : 'Using the built-in sound'}
                  </span>
                  <div className="patch-role-actions">
                    <button
                      onClick={() => assignCurrentPatchToRole(role)}
                      disabled={!currentPatchAssignPath}
                      title={`Use the current sound for ${roleMeta.label}.`}
                    >
                      Use this sound
                    </button>
                    <button onClick={() => setSongPatch(role, null)} title={`Switch ${roleMeta.label} back to the built-in sound.`}>
                      Use built-in
                    </button>
                  </div>
                </div>
              );
            })}
          </div>
          <label>
            Sound name
            <input
              value={patchDocument.patch.name}
              onChange={(event) =>
                updatePatchDocument((current) => ({
                  ...clonePatchDocument(current),
                  patch: {
                    ...clonePatchDocument(current).patch,
                    name: event.target.value,
                  },
                }))
              }
            />
          </label>
        </>
      ) : null}
      {patchWorkspaceTab === 'nodes' ? (
        <div className="patch-grid">
          <CollapsibleSection
            title="Building blocks"
            headingLevel={3}
            className="patch-column"
            summary={countLabel(patchNodeEntries.length, 'block')}
            expanded={expandedPanels.patchNodes}
            onToggle={() => togglePanel('patchNodes')}
            actions={
              <>
                <select
                  value={newPatchNodeType}
                  onChange={(event) => setNewPatchNodeType(event.target.value as PatchNodeType)}
                  title="Choose which kind of block to add next."
                >
                  {PATCH_NODE_TYPE_OPTIONS.map((option) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
                <button onClick={() => addPatchNode()} title="Add a new block to this sound.">
                  Add block
                </button>
              </>
            }
          >
            <div className="patch-chip-list">
              {patchNodeEntries.map(([nodeId, node]) => (
                <button
                  key={nodeId}
                  className={nodeId === selectedPatchNodeId ? 'section-chip selected' : 'section-chip'}
                  onClick={() => setSelectedPatchNodeId(nodeId)}
                >
                  <span>{nodeId}</span>
                  <span className="section-chip-meta">{node.type}</span>
                </button>
              ))}
            </div>
            {selectedPatchNode ? (
              <div className="patch-inspector">
                <label>
                  Block id
                  <input value={selectedPatchNodeId} onChange={(event) => renameSelectedPatchNode(event.target.value)} />
                </label>
                <label>
                  Block type
                  <input value={selectedPatchNode.type} disabled />
                </label>
                {selectedPatchNode.type === 'oscillator' ? (
                  <div className="field-grid compact-grid">
                    <label>
                      Waveform
                      <select
                        value={selectedPatchNode.waveform}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            waveform: event.target.value as Extract<PatchNode, { type: 'oscillator' }>['waveform'],
                          }))
                        }
                      >
                        <option value="sine">sine</option>
                        <option value="saw">saw</option>
                        <option value="square">square</option>
                        <option value="triangle">triangle</option>
                      </select>
                    </label>
                    <label>
                      Level
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        value={selectedPatchNode.level}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            level: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Octaves
                      <input
                        type="number"
                        step={1}
                        value={selectedPatchNode.pitch.octaves}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            pitch: {
                              ...(node as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                              octaves: Math.round(Number(event.target.value) || 0),
                            },
                          }))
                        }
                      />
                    </label>
                    <label>
                      Semitones
                      <input
                        type="number"
                        step={0.01}
                        value={selectedPatchNode.pitch.semitones}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            pitch: {
                              ...(node as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                              semitones: Number(event.target.value) || 0,
                            },
                          }))
                        }
                      />
                    </label>
                    <label>
                      Cents
                      <input
                        type="number"
                        step={0.01}
                        value={selectedPatchNode.pitch.cents}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            pitch: {
                              ...(node as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                              cents: Number(event.target.value) || 0,
                            },
                          }))
                        }
                      />
                    </label>
                    <label>
                      Hz offset
                      <input
                        type="number"
                        step={0.01}
                        value={selectedPatchNode.pitch.hzOffset}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            pitch: {
                              ...(node as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                              hzOffset: Number(event.target.value) || 0,
                            },
                          }))
                        }
                      />
                    </label>
                  </div>
                ) : null}
                {selectedPatchNode.type === 'noise' ? (
                  <div className="field-grid compact-grid">
                    <label>
                      Color
                      <select
                        value={selectedPatchNode.color}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'noise' }>),
                            color: event.target.value as Extract<PatchNode, { type: 'noise' }>['color'],
                          }))
                        }
                      >
                        <option value="white">white</option>
                        <option value="pink">pink</option>
                        <option value="mix">mix</option>
                      </select>
                    </label>
                    <label>
                      Mix
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        max={1}
                        value={selectedPatchNode.mix}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'noise' }>),
                            mix: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Level
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        value={selectedPatchNode.level}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'noise' }>),
                            level: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                  </div>
                ) : null}
                {selectedPatchNode.type === 'envelope' ? (
                  <div className="field-grid compact-grid">
                    {(['attack', 'decay', 'sustain', 'release'] as const).map((field) => (
                      <label key={field}>
                        {field}
                        <input
                          type="number"
                          step={0.01}
                          min={0}
                          max={field === 'sustain' ? 1 : undefined}
                          value={selectedPatchNode[field]}
                          onChange={(event) =>
                            updatePatchNode(selectedPatchNodeId, (node) => ({
                              ...(node as Extract<PatchNode, { type: 'envelope' }>),
                              [field]: Number(event.target.value) || 0,
                            }))
                          }
                        />
                      </label>
                    ))}
                  </div>
                ) : null}
                {selectedPatchNode.type === 'filter' ? (
                  <div className="field-grid compact-grid">
                    <label>
                      Mode
                      <select
                        value={selectedPatchNode.mode}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            mode: event.target.value as Extract<PatchNode, { type: 'filter' }>['mode'],
                          }))
                        }
                      >
                        <option value="lowpass">lowpass</option>
                        <option value="highpass">highpass</option>
                        <option value="bandpass">bandpass</option>
                      </select>
                    </label>
                    <label>
                      Cutoff Hz
                      <input
                        type="number"
                        step={1}
                        min={1}
                        value={selectedPatchNode.cutoffHz}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            cutoffHz: Number(event.target.value) || 1,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Q
                      <input
                        type="number"
                        step={0.01}
                        min={0.01}
                        value={selectedPatchNode.q}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            q: Number(event.target.value) || 0.01,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Slope
                      <select
                        value={selectedPatchNode.slope}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            slope: event.target.value as Extract<PatchNode, { type: 'filter' }>['slope'],
                          }))
                        }
                      >
                        <option value="12">12</option>
                        <option value="24">24</option>
                        <option value="36">36</option>
                        <option value="48">48</option>
                      </select>
                    </label>
                    <label>
                      Key track
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        max={1}
                        value={selectedPatchNode.keyTrack}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            keyTrack: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Env amount oct
                      <input
                        type="number"
                        step={0.01}
                        value={selectedPatchNode.envAmountOct}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            envAmountOct: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Q env amount
                      <input
                        type="number"
                        step={0.01}
                        value={selectedPatchNode.qEnvAmount}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            qEnvAmount: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                  </div>
                ) : null}
                {selectedPatchNode.type === 'output' ? (
                  <div className="field-grid compact-grid">
                    <label>
                      Gain
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        value={selectedPatchNode.gain}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'output' }>),
                            gain: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Layer spread
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        max={1}
                        value={selectedPatchNode.layerSpread}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'output' }>),
                            layerSpread: Number(event.target.value) || 0,
                          }))
                        }
                      />
                    </label>
                    <label>
                      Play mode
                      <select
                        value={selectedPatchNode.playMode}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'output' }>),
                            playMode: event.target.value as Extract<PatchNode, { type: 'output' }>['playMode'],
                          }))
                        }
                      >
                        <option value="poly">poly</option>
                        <option value="monolegato">monolegato</option>
                      </select>
                    </label>
                    <label>
                      Poly max
                      <input
                        type="number"
                        step={1}
                        min={1}
                        value={selectedPatchNode.polyMax}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'output' }>),
                            polyMax: Math.round(Number(event.target.value) || 1),
                          }))
                        }
                      />
                    </label>
                  </div>
                ) : null}
                <button className="danger" onClick={() => removePatchNode(selectedPatchNodeId)}>
                  Remove block
                </button>
              </div>
            ) : (
              <div className="hint">Select a block to inspect it.</div>
            )}
          </CollapsibleSection>
        </div>
      ) : null}
      {patchWorkspaceTab === 'connections' ? (
        <div className="patch-grid">
          <CollapsibleSection
            title="Routing"
            headingLevel={3}
            className="patch-column"
            summary={countLabel(patchConnectionEntries.length, 'connection')}
            expanded={expandedPanels.patchConnections}
            onToggle={() => togglePanel('patchConnections')}
            actions={
              <>
                <select
                  value={newPatchConnectionKind}
                  onChange={(event) => setNewPatchConnectionKind(event.target.value as PatchConnectionKind)}
                  title="Choose which kind of connection to add next."
                >
                  {PATCH_CONNECTION_KIND_OPTIONS.map((option) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
                <button onClick={() => addPatchConnection()} title="Create a new routing entry in the inspector.">
                  Add route
                </button>
              </>
            }
          >
            <div className="patch-chip-list">
              {patchConnectionEntries.map(([connectionId, connection]) => (
                <button
                  key={connectionId}
                  className={connectionId === selectedPatchConnectionId ? 'section-chip selected' : 'section-chip'}
                  onClick={() => setSelectedPatchConnectionId(connectionId)}
                >
                  <span>{connectionId}</span>
                  <span className="section-chip-meta">{connection.kind}</span>
                </button>
              ))}
            </div>
            {selectedPatchConnection ? (
              <div className="patch-inspector">
                <label>
                  Source
                  <select
                    value={selectedPatchConnection.from}
                    onChange={(event) =>
                      updatePatchConnection(selectedPatchConnectionId, (connection) => ({
                        ...connection,
                        from: event.target.value,
                      }))
                    }
                  >
                    {patchNodeEntries.map(([nodeId]) => (
                      <option key={`from-${nodeId}`} value={nodeId}>
                        {nodeId}
                      </option>
                    ))}
                  </select>
                </label>
                <label>
                  Destination
                  <select
                    value={selectedPatchConnection.to}
                    onChange={(event) =>
                      updatePatchConnection(selectedPatchConnectionId, (connection) => ({
                        ...connection,
                        to: event.target.value,
                      }))
                    }
                  >
                    {patchNodeEntries.map(([nodeId]) => (
                      <option key={`to-${nodeId}`} value={nodeId}>
                        {nodeId}
                      </option>
                    ))}
                  </select>
                </label>
                <label>
                  Connection type
                  <select
                    value={selectedPatchConnection.kind}
                    onChange={(event) =>
                      updatePatchConnection(selectedPatchConnectionId, (connection) => ({
                        ...connection,
                        kind: event.target.value as PatchConnectionKind,
                      }))
                    }
                  >
                    {PATCH_CONNECTION_KIND_OPTIONS.map((option) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </select>
                </label>
                <button className="danger" onClick={() => removePatchConnection(selectedPatchConnectionId)}>
                  Remove route
                </button>
              </div>
            ) : (
              <div className="hint">Select a route to inspect it.</div>
            )}
          </CollapsibleSection>
        </div>
      ) : null}
    </CollapsibleSection>
  );

  const melodyWorkspace = selectedSection ? (
    <section className="melody-panel melody-panel-primary">
      {document.sections.length === 0 ? (
        <div className="empty-state compact-empty-state">No sections yet.</div>
      ) : (
        <div className="section-strip-row">
          <div className="section-tab-list">
            {document.sections.map((section, index) => (
              <button
                key={`${section.name}-${index}-tab`}
                data-section-drop-target={section.name}
                data-section-drop-axis="horizontal"
                className={[
                  'section-tab',
                  section.name === selectedSectionName ? 'active' : '',
                  draggedSectionName === section.name ? 'dragging' : '',
                  sectionDropHint?.sectionName === section.name ? `drop-${sectionDropHint.position}` : '',
                ]
                  .filter(Boolean)
                  .join(' ')}
                onClick={() => selectSection(section.name)}
                title={`Open ${section.name} to edit melody and chord progression.`}
              >
                <span className="section-tab-line">
                  <span className="section-tab-name">{section.name}</span>
                  <span className="section-tab-meta">
                    {countLabel(section.melody.length, 'note')} · {countLabel(sectionChordRegions(document, section).length, 'chord change')}
                 </span>
                 </span>
                <span className="section-tab-actions">
                  <span
                    className="section-delete-button"
                    onClick={(event) => {
                      event.stopPropagation();
                      setShowSectionDetails(false);
                      setShowProgressionEditor(false);
                      removeSection(section.name);
                    }}
                    title={`Delete ${section.name}.`}
                  >
                    ×
                  </span>
                  <span
                    className="section-drag-handle"
                    onPointerDown={(event) => beginSectionPointerDrag(event, section.name)}
                    title={`Drag to move ${section.name} in the arrangement.`}
                  >
                    ⋮⋮
                  </span>
                </span>
              </button>
            ))}
          </div>
          <div className="section-strip-actions inline-actions">
            <button
              className={showSectionDetails ? 'toggle-button active' : 'toggle-button'}
              onClick={() => setShowSectionDetails((value) => !value)}
              title={selectedSection ? `Edit details for ${selectedSection.name}.` : 'Select a section first.'}
              disabled={!selectedSection}
            >
              Section details
            </button>
            <button onClick={() => addSection()} title="Create another section and keep writing.">
              New section
            </button>
            <button onClick={() => void playPreview(true)} title="Loop the current section while you work.">
              Loop section
            </button>
          </div>
        </div>
      )}
      {renderSectionDetailsPopover()}

      <div className="chord-timeline-row">
        <div className="workspace-toolbar-group melody-grid-controls">
          <button
            className={showProgressionEditor ? 'toggle-button active' : 'toggle-button'}
            onClick={() => setShowProgressionEditor((value) => !value)}
            title={selectedSection ? `Edit the chord progression for ${selectedSection.name}.` : 'Select a section first.'}
            disabled={!selectedSection}
          >
            Chord changes
          </button>
          <button onClick={() => addChordChange()} title="Add another chord change to this phrase.">
            Add change
          </button>
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
            <span>Preview sound</span>
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
              min={MIN_BEAT_WIDTH}
              max={MAX_BEAT_WIDTH}
              step={2}
              value={beatWidth}
              onChange={(event) => setBeatWidth(Number(event.target.value))}
            />
          </label>
        </div>
      </div>
      {renderProgressionEditorPopover()}

      <MelodyGrid
        beatsPerBar={activeBeatsPerBar}
        totalBeats={totalBeats}
        phraseBeats={phraseBeats}
        phraseCount={selectedSection.phraseCount}
        beatWidth={beatWidth}
        minBeatWidth={MIN_BEAT_WIDTH}
        maxBeatWidth={MAX_BEAT_WIDTH}
        quantizeStep={snapStep}
        notes={selectedSection.melody}
        chordRegions={selectedSectionChordRegionSpans.map((region, index) => ({
          ...region,
          romanLabel: chordRomans[index] ?? region.symbol,
        }))}
        overlayNotes={overlayNotes}
        selectedNoteId={selectedNoteId}
        selectedChordRegionId={selectedChordRegion?.id ?? null}
        onSelectNote={setSelectedNoteId}
        onSelectChordRegion={setSelectedChordRegionId}
        onUpsertChordRegion={(region) =>
          setSelectedSectionChordRegions(
            selectedSectionChordRegions.map((entry) => (entry.id === region.id ? { ...entry, ...region } : entry)),
          )
        }
        onBeatWidthChange={setBeatWidth}
        onUpsertNote={(note) => upsertNote(selectedSection.name, note)}
        onPreviewNote={(midi) => void previewPianoNote(midi)}
      />
    </section>
  ) : (
    <section className="panel empty-state melody-panel melody-panel-primary">
      <div className="panel-heading">
        <h2>Melody</h2>
        <span className="panel-summary">Create a section to start writing.</span>
      </div>
      <div className="inline-actions">
        <button onClick={() => addSection()} title="Create your first section.">
          Create first section
        </button>
      </div>
    </section>
  );

  function renderSectionDetailsPopover() {
    return selectedSection && showSectionDetails ? (
      <section className="inline-editor-popover section-editor">
        <div className="section-details-grid">
        <label className="section-detail-field section-detail-compact">
          Bars/phrase
          <input
            type="number"
            min={1}
            max={16}
            value={selectedSection.barsPerPhrase}
            onChange={(event) => setSelectedSectionField('barsPerPhrase', Number(event.target.value) || 1)}
          />
        </label>
        <label className="section-detail-field section-detail-compact">
          Phrases
          <input
            type="number"
            min={1}
            max={16}
            value={selectedSection.phraseCount}
            onChange={(event) => setSelectedSectionField('phraseCount', Number(event.target.value) || 1)}
          />
        </label>
        <label className="section-detail-field section-detail-medium">
          Tempo
          <input
            type="number"
            min={20}
            max={280}
            value={selectedSection.tempoBpm ?? ''}
            placeholder="Song tempo"
            onChange={(event) =>
              setSelectedSectionField('tempoBpm', event.target.value === '' ? null : Number(event.target.value))
            }
          />
        </label>
        <label className="section-detail-field section-detail-medium">
          Beats/bar
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
        <label className="section-detail-field section-detail-medium">
          Beat unit
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
        <label className="section-detail-field section-detail-note">
          Mood note
          <input value={selectedSection.mood} onChange={(event) => setSelectedSectionField('mood', event.target.value)} />
        </label>
        <label className="section-detail-field section-detail-note">
          Feel note
          <input value={selectedSection.feel} onChange={(event) => setSelectedSectionField('feel', event.target.value)} />
        </label>
        </div>
      </section>
    ) : null;
  }

  function renderProgressionEditorPopover() {
    return selectedSection && showProgressionEditor ? (
      <section className="inline-editor-popover chord-progression-editor">
        {selectedChordRegion ? (
          <div className="section-details-grid chord-region-details-grid">
            <label className="section-detail-field section-detail-note">
              Chord
              <input
                value={selectedChordRegion.symbol}
                onChange={(event) =>
                  updateSelectedChordRegion((region) => ({
                    ...region,
                    symbol: event.target.value,
                  }))
                }
                placeholder="Am(add9)"
              />
            </label>
            <label className="section-detail-field section-detail-medium">
              Start
              <input
                type="number"
                min={0}
                max={Math.max(0, phraseBeats - snapStep)}
                step={snapStep}
                value={selectedChordRegion.startBeat}
                disabled={selectedSectionChordRegions[0]?.id === selectedChordRegion.id}
                onChange={(event) =>
                  updateSelectedChordRegion((region) => ({
                    ...region,
                    startBeat:
                      selectedSectionChordRegions[0]?.id === region.id
                        ? 0
                        : quantizeBeat(Number(event.target.value) || snapStep, snapStep),
                  }))
                }
              />
            </label>
            <div className="section-detail-field section-detail-compact chord-region-readout">
              <span>Roman</span>
              <strong>{selectedChordRegionRoman ?? '—'}</strong>
            </div>
            <div className="section-detail-field section-detail-medium chord-region-readout">
              <span>Span</span>
              <strong>
                {selectedChordRegionSpan ? `${selectedChordRegionSpan.startBeat}–${selectedChordRegionSpan.endBeat}` : '—'}
              </strong>
            </div>
            <button
              className="inline-remove-button"
              onClick={() => removeSelectedChordChange()}
              disabled={selectedSectionChordRegions.length <= 1}
              title="Remove this chord change."
            >
              ×
            </button>
          </div>
        ) : null}
      </section>
    ) : null;
  }

  const backgroundPartsPanel = (
    <CollapsibleSection
      title="Arrangement preview"
      summary={accompanimentSummary}
      expanded={expandedPanels.arrangementPreview}
      onToggle={() => togglePanel('arrangementPreview')}
      className="generated-strip"
    >
      <div className="strip-head">
        <div className="panel-heading">
          <span className="panel-summary">Auto-generated backing notes so you can hear the full section while writing melody.</span>
        </div>
        <div className="inline-actions">
          {selectedTimelineSection ? <span className="toolbar-badge">{countLabel(accompanimentLegend.length, 'part')}</span> : null}
          <button
            className={showAccompaniment ? 'toggle-button active' : 'toggle-button'}
            onClick={() => setShowAccompaniment((value) => !value)}
            title="Show or hide the arrangement preview notes on the melody grid."
          >
            {arrangementPreviewToggleLabel}
          </button>
          <button
            onClick={() => void refreshAccompaniment()}
            disabled={accompanimentLoading}
            title="Update the auto-generated arrangement preview after melody or harmony edits."
          >
            {accompanimentLoading ? 'Updating…' : 'Update preview'}
          </button>
        </div>
      </div>
      {selectedTimelineSection && showAccompaniment ? (
        <div className="legend-list">
          {accompanimentLegend.map((entry) => (
            <span key={entry.name} className="legend-chip">
              <span className="legend-dot" style={{ background: entry.color, borderColor: entry.accentColor }} />
              {entry.name} ({entry.count})
            </span>
          ))}
        </div>
      ) : (
        <div className="hint">
          {showAccompaniment
            ? 'Use Update preview to regenerate the backing notes for this section.'
            : 'Turn on Show backing notes if you want to see the arrangement preview under your melody.'}
        </div>
      )}
      {accompanimentError ? <div className="validation-inline">{accompanimentError}</div> : null}
    </CollapsibleSection>
  );

  const noteInspectorPanel = selectedSection && selectedNote ? (
      <section className="note-editor">
        <div className="panel-header">
          <div className="panel-heading">
            <h3>Selected note</h3>
            <span className="panel-summary">Edits stay snapped to the current grid and can be previewed instantly.</span>
          </div>
          <span className="toolbar-badge note-badge">{midiToNoteName(selectedNote.note)}</span>
        </div>
        <div className="note-summary-strip">
          <span className="toolbar-badge">Beat {selectedNote.beat}</span>
          <span className="toolbar-badge">Dur {selectedNote.duration}</span>
          <span className="toolbar-badge">Vel {selectedNote.velocity.toFixed(2)}</span>
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
              onChange={(event) => updateSelectedNote((note) => ({ ...note, velocity: Number(event.target.value) || 0 }))}
            />
          </label>
        </div>
        <div className="shortcut-strip">
          <span>Click piano keys to preview {auditionInstrumentLabel}</span>
          <span>⌘/Ctrl+Z undo</span>
          <span>⇧⌘/Ctrl+Z redo</span>
          <span>⌘/Ctrl+C then ⌘/Ctrl+V duplicate</span>
          <span>⌘/Ctrl+X cut</span>
          <span>Space play/pause</span>
          <span>Backspace delete</span>
          <span>Arrows nudge</span>
          <span>Shift+Arrows large nudge</span>
        </div>
      </section>
  ) : null;

  return (
    <div className="app-shell">
      <header className="topbar">
        <div className="toolbar-group topbar-file-group">
          <div
            className="file-pill project-pill"
            title={currentPath ? `${fileStatusLabel}: ${currentPath}` : 'Grey means this is a new untitled project.'}
          >
            <span className={`dirty-indicator ${fileStatusTone}`} />
            <div className="project-meta">
              <strong className="project-path">{currentFileDisplay}</strong>
              <span className={`project-status ${fileStatusTone}`}>{fileStatusLabel}</span>
            </div>
          </div>
        </div>

        <div className="toolbar-group transport-group">
          <button
            className="transport-button primary"
            title={playback.running && playback.transportState === 'playing' ? 'Pause playback' : 'Play or resume playback'}
            onClick={() => void togglePlayback()}
          >
            {playback.running && playback.transportState === 'playing' ? '⏸' : '▶'}
          </button>
          <button className="transport-button" title="Stop playback" onClick={() => void stopPreview()} disabled={!playback.running}>
            ■
          </button>
          <button
            className={playback.loopSectionName ? 'transport-button active' : 'transport-button'}
            title={selectedSection ? `Loop the selected section: ${selectedSection.name}` : 'Select a section before looping.'}
            onClick={() => void playPreview(true)}
            disabled={!selectedSection}
          >
            ↻
          </button>
        </div>

        <div className="toolbar-group topbar-song-meta">
          <label className="topbar-field" title="Choose the style used for generated parts and melody previews.">
            <span>Genre</span>
            <select value={document.song.genre} onChange={(event) => setSongField('genre', event.target.value)}>
              {songGenreOptions.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>
          <label className="topbar-field" title="Set the overall feeling for the song.">
            <span>Mood</span>
            <select value={document.song.mood} onChange={(event) => setSongField('mood', event.target.value)}>
              {songMoodOptions.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>
          <label className="topbar-field topbar-field-small" title="Set the default tempo in beats per minute.">
            <span>Tempo</span>
            <input
              type="number"
              min={20}
              max={280}
              value={document.song.tempoBpm}
              onChange={(event) => setSongField('tempoBpm', Number(event.target.value) || 100)}
            />
          </label>
          <label className="topbar-field topbar-field-small" title="Set the default number of beats in each bar.">
            <span>Beats/Bar</span>
            <input
              type="number"
              min={1}
              max={16}
              value={document.song.beatsPerBar}
              onChange={(event) => setSongField('beatsPerBar', Number(event.target.value) || 4)}
            />
          </label>
          <label className="topbar-field topbar-field-small" title="Choose which note value counts as one beat.">
            <span>Beat Unit</span>
            <select value={document.song.beatUnit} onChange={(event) => setSongField('beatUnit', Number(event.target.value) || 4)}>
              {BEAT_UNIT_OPTIONS.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>
        </div>
      </header>

      {editorMode === 'patch' ? (
        <div className="status-strip">
          <span>Sound editor open</span>
          <span>{transportLabel(playback)}</span>
          <span>{patchValidationOk === false ? 'Sound checks need attention' : patchValidationMessage}</span>
          <span>{countLabel(patchNodeEntries.length, 'block')} · {countLabel(patchConnectionEntries.length, 'connection')}</span>
          <span>{activePatchRoleMeta ? `Editing ${activePatchRoleMeta.label}` : currentPatchLabel}</span>
        </div>
      ) : null}

      {editorMode === 'arrangement' ? (
        <main className="arrangement-workspace">
          <section className="arrangement-main-column">
            {melodyWorkspace}

            <div className="arrangement-support-row">
              {noteInspectorPanel}
              {backgroundPartsPanel}
            </div>
          </section>

          <aside className="arrangement-sidebar-column">
            <aside className="sidebar panel arrangement-sidebar-panel">
              <CollapsibleSection
                title="Instruments"
                summary={instrumentsSummary}
                expanded={expandedPanels.sound}
                onToggle={() => togglePanel('sound')}
              >
                <div className="instrument-role-list">
                  {PATCH_ROLE_OPTIONS.map((role) => {
                    const roleMeta = PATCH_ROLE_META[role];
                    const assignedPath = document.song.patches[role];
                    return (
                      <div key={role} className="instrument-row" title={roleMeta.description}>
                        <div className="instrument-row-summary">
                          <strong>{roleMeta.label}</strong>
                          <span className="instrument-row-tag">{assignedPath ? 'custom' : 'built-in'}</span>
                        </div>
                        <label className="instrument-row-select">
                          <span className="sr-only">Sound choice for {roleMeta.label}</span>
                          <select value={assignedPath ?? ''} onChange={(event) => setSongPatch(role, event.target.value || null)}>
                            <option value="">Built-in sound</option>
                            {patchSelectOptions.map((entry) => (
                              <option key={`${role}-${entry.path}`} value={entry.assignPath}>
                                {entry.label}
                              </option>
                            ))}
                          </select>
                        </label>
                        <button className="instrument-row-edit" onClick={() => void openPatchEditorForRole(role)} title={`Open the full sound editor for ${roleMeta.label}.`}>
                          Edit
                        </button>
                      </div>
                    );
                  })}
                </div>
                <div className="hint">Each choice is saved with the song. Open a sound to fine-tune it in the full sound editor.</div>
              </CollapsibleSection>

              <CollapsibleSection
                title="Song flow"
                summary={document.sections.length === 0 ? 'No arrangement yet' : countLabel(document.sections.length, 'step')}
                expanded={expandedPanels.form}
                onToggle={() => togglePanel('form')}
              >
                <ol className="form-list">
                  {document.sections.map((section) => (
                    <li
                      key={section.name}
                      data-section-drop-target={section.name}
                      data-section-drop-axis="vertical"
                      className={[
                        draggedSectionName === section.name ? 'dragging' : '',
                        sectionDropHint?.sectionName === section.name ? `drop-${sectionDropHint.position}` : '',
                      ]
                        .filter(Boolean)
                        .join(' ')}
                    >
                      <span
                        className="section-drag-handle flow-drag-handle"
                        onPointerDown={(event) => beginSectionPointerDrag(event, section.name)}
                        title={`Drag to move ${section.name} in the song flow.`}
                      >
                        ⋮⋮
                      </span>
                      <span>{section.name}</span>
                      <div className="inline-actions">
                        <button onClick={() => moveSection(section.name, -1)}>↑</button>
                        <button onClick={() => moveSection(section.name, 1)}>↓</button>
                      </div>
                    </li>
                  ))}
                </ol>
              </CollapsibleSection>
            </aside>
            <aside className="sidebar panel arrangement-tools-panel">{projectToolsSection}</aside>
          </aside>
        </main>
      ) : (
        <main className="patch-editor-layout">
          <section className="panel sound-editor-panel">
            <div className="panel-header sound-editor-header">
              <div className="panel-heading">
                <h2>Sound editor</h2>
                <span className="panel-summary">
                  {activePatchRoleMeta
                    ? `${activePatchRoleMeta.label} · ${activePatchRoleMeta.description}`
                    : 'Build and route the current sound here.'}
                </span>
              </div>
              <div className="inline-actions">
                {activePatchRole ? (
                  <button
                    onClick={() => assignCurrentPatchToRole(activePatchRole)}
                    disabled={!currentPatchAssignPath}
                    title={`Use this sound for ${activePatchRoleMeta?.label}.`}
                  >
                    Use for {activePatchRoleMeta?.label}
                  </button>
                ) : null}
                <button onClick={() => void auditionCurrentPatch()} title="Play a short preview through the current sound.">
                  Preview
                </button>
                <button onClick={() => void validateCurrentPatch()} title="Check the current sound before saving or assigning it.">
                  Check sound
                </button>
                <button onClick={() => returnToArrangement()} title="Return to the arrangement view.">
                  Back to arrangement
                </button>
              </div>
            </div>

            <div className="patch-graph-toolbar sound-editor-toolbar">
              <div className="patch-graph-status">
                <span className="toolbar-badge">{currentPatchLabel}</span>
                <span className="toolbar-badge">{countLabel(patchNodeEntries.length, 'block')}</span>
                <span className="toolbar-badge">{countLabel(patchConnectionEntries.length, 'connection')}</span>
                {activePatchRoleMeta ? <span className="toolbar-badge">Editing {activePatchRoleMeta.label}</span> : null}
              </div>
              <div className="inline-actions">
                <button
                  className={patchGraphToolMode === 'select' ? 'toggle-button active' : 'toggle-button'}
                  onClick={() => setPatchGraphToolMode('select')}
                >
                  Select
                </button>
                <button
                  className={patchGraphToolMode === 'wire' ? 'toggle-button active' : 'toggle-button'}
                  onClick={() => setPatchGraphToolMode('wire')}
                >
                  Connect
                </button>
                <label className="toolbar-control">
                  <span>Connection type</span>
                  <select value={newPatchConnectionKind} onChange={(event) => setNewPatchConnectionKind(event.target.value as PatchConnectionKind)}>
                    {PATCH_CONNECTION_KIND_OPTIONS.map((option) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </select>
                </label>
              </div>
            </div>

            <div className="patch-graph-toolbar">
              <span className="hint">Build the sound by adding blocks, then switch to Connect to wire them together.</span>
              <div className="inline-actions">
                {PATCH_NODE_TYPE_OPTIONS.map((option) => (
                  <button key={option.value} onClick={() => addPatchNodeOfType(option.value)}>
                    + {option.label}
                  </button>
                ))}
              </div>
            </div>

            <PatchGraphCanvas
              patchDocument={patchDocument}
              selectedNodeId={selectedPatchNodeId}
              selectedConnectionId={selectedPatchConnectionId}
              toolMode={patchGraphToolMode}
              connectionKind={newPatchConnectionKind}
              onSelectNode={setSelectedPatchNodeId}
              onSelectConnection={setSelectedPatchConnectionId}
              onCreateConnection={connectPatchNodes}
              onUpdateNode={updatePatchNode}
              onUpdateConnection={updatePatchConnection}
              onRemoveConnection={removePatchConnection}
            />
          </section>

          <aside className="sidebar panel patch-editor-sidebar">
            {soundSetupSection}
            {projectToolsSection}
          </aside>
        </main>
      )}

      <footer className="footer-strip">
        <span>{statusMessage}</span>
        <span>⌘/Ctrl+N New · ⌘/Ctrl+O Open · ⌘/Ctrl+S Save · ⇧⌘/Ctrl+S Save As · Space Play/Pause</span>
      </footer>
    </div>
  );
}

export default App;
