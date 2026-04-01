import {
  type CSSProperties,
  type PointerEvent as ReactPointerEvent,
  type ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react';
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
  buildChordPalette,
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
import type { ValidationIssue } from './lib/validation';
import { useStudioStore } from './store/studioStore';

type FilePayload = {
  path: string;
  contents: string;
};

type SaveSongResponse = {
  path: string;
};

type ChordPickerAnchor = {
  x: number;
  y: number;
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
type UtilityTrayTab = 'preview' | 'note';

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
  seed?: number;
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

type OperationChannel = 'previewGeneration' | 'playback' | 'soundAudition' | 'engineCheck';

type OperationAction =
  | 'refresh-preview'
  | 'play-preview'
  | 'pause-preview'
  | 'resume-preview'
  | 'stop-preview'
  | 'audition-sound'
  | 'validate-sound';

type OperationSeverity = 'error' | 'success' | 'info';

type OperationFeedback = {
  id: string;
  channel: OperationChannel;
  title: string;
  message: string;
  source: string;
  severity: OperationSeverity;
  action: OperationAction | null;
  timestamp: number;
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
const MAX_PREVIEW_SEED = 2_147_483_647;
const CHORD_SNAP_STEP = 0.5;
const NOTE_CLIPBOARD_PREFIX = 'IDOU_NOTE:';
const OPERATION_HISTORY_LIMIT = 12;

const EMPTY_OPERATION_FEEDBACK: Record<OperationChannel, OperationFeedback | null> = {
  previewGeneration: null,
  playback: null,
  soundAudition: null,
  engineCheck: null,
};

const INSTRUMENT_COLORS: Record<string, string> = {
  drums: 'rgba(239, 68, 68, 0.62)',
  bass: 'rgba(250, 204, 21, 0.64)',
  pad: 'rgba(34, 197, 94, 0.58)',
  arp: 'rgba(59, 130, 246, 0.6)',
  lead: 'rgba(168, 85, 247, 0.6)',
  'auto-drums': 'rgba(239, 68, 68, 0.62)',
};

const INSTRUMENT_ACCENTS: Record<string, string> = {
  drums: 'rgba(254, 202, 202, 1)',
  bass: 'rgba(253, 224, 71, 0.98)',
  pad: 'rgba(187, 247, 208, 0.98)',
  arp: 'rgba(191, 219, 254, 0.98)',
  lead: 'rgba(233, 213, 255, 0.98)',
  'auto-drums': 'rgba(254, 202, 202, 1)',
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
    'rgba(59, 130, 246, 0.6)',
    'rgba(250, 204, 21, 0.64)',
    'rgba(34, 197, 94, 0.58)',
    'rgba(239, 68, 68, 0.62)',
    'rgba(168, 85, 247, 0.6)',
  ];
  let hash = 0;
  for (const char of name) {
    hash = (hash * 31 + char.charCodeAt(0)) >>> 0;
  }
  return palette[hash % palette.length];
}

function fallbackInstrumentAccent(name: string): string {
  const palette = [
    'rgba(96, 165, 250, 0.96)',
    'rgba(253, 224, 71, 0.98)',
    'rgba(74, 222, 128, 0.96)',
    'rgba(248, 113, 113, 0.98)',
    'rgba(196, 181, 253, 0.98)',
  ];
  let hash = 0;
  for (const char of name) {
    hash = (hash * 31 + char.charCodeAt(0)) >>> 0;
  }
  return palette[hash % palette.length];
}

function normalizeInstrumentVisualKey(name: string): string {
  const normalized = name.trim().toLowerCase();
  if (normalized.includes('drum')) {
    return 'drums';
  }
  if (normalized.includes('bass')) {
    return 'bass';
  }
  if (normalized.includes('pad')) {
    return 'pad';
  }
  if (normalized.includes('arp')) {
    return 'arp';
  }
  if (normalized.includes('lead')) {
    return 'lead';
  }
  return normalized;
}

function instrumentColor(name: string): string {
  const key = normalizeInstrumentVisualKey(name);
  return INSTRUMENT_COLORS[key] ?? fallbackInstrumentColor(key);
}

function instrumentAccent(name: string): string {
  const key = normalizeInstrumentVisualKey(name);
  return INSTRUMENT_ACCENTS[key] ?? fallbackInstrumentAccent(key);
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

function validationActionLabel(issue: ValidationIssue): string {
  switch (issue.focus?.kind) {
    case 'song-section':
      return 'Focus section';
    case 'sound-node':
      return 'Focus block';
    case 'sound-connection':
      return 'Focus route';
    case 'sound':
      return 'Review sound';
    case 'song':
    default:
      return 'Review';
  }
}

function operationActionLabel(action: OperationAction): string {
  switch (action) {
    case 'refresh-preview':
      return 'Retry preview';
    case 'play-preview':
      return 'Play again';
    case 'pause-preview':
      return 'Retry pause';
    case 'resume-preview':
      return 'Retry resume';
    case 'stop-preview':
      return 'Retry stop';
    case 'audition-sound':
      return 'Retry preview';
    case 'validate-sound':
      return 'Run check again';
  }
}

function formatOperationTimestamp(timestamp: number): string {
  return new Date(timestamp).toLocaleTimeString([], {
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
  });
}

function createOperationFeedback(
  channel: OperationChannel,
  severity: OperationSeverity,
  title: string,
  message: string,
  source: string,
  action: OperationAction | null,
): OperationFeedback {
  return {
    id: `${channel}:${Date.now()}:${Math.random().toString(36).slice(2, 8)}`,
    channel,
    severity,
    title,
    message,
    source,
    action,
    timestamp: Date.now(),
  };
}

type ValidationIssuePanelProps = {
  title: string;
  subtitle: string;
  issues: ValidationIssue[];
  emptyMessage: string;
  onFocusIssue: (issue: ValidationIssue) => void;
  canFocusIssue: (issue: ValidationIssue) => boolean;
  footer?: ReactNode;
};

function ValidationIssuePanel({
  title,
  subtitle,
  issues,
  emptyMessage,
  onFocusIssue,
  canFocusIssue,
  footer,
}: ValidationIssuePanelProps) {
  const hasIssues = issues.length > 0;

  return (
    <section className={`validation-panel ${hasIssues ? 'error' : 'ok'}`}>
      <div className="validation-panel-head">
        <div className="validation-panel-copy">
          <strong className="validation-panel-title">{title}</strong>
          <span className="validation-panel-subtitle">{subtitle}</span>
        </div>
        <span className={`validation-count ${hasIssues ? 'error' : 'success'}`}>
          {hasIssues ? countLabel(issues.length, 'issue') : 'Ready'}
        </span>
      </div>

      {hasIssues ? (
        <ul className="validation-item-list">
          {issues.map((issue) => {
            const canFocus = canFocusIssue(issue);
            return (
              <li key={issue.id} className={`validation-item ${issue.severity}`}>
                <div className="validation-item-body">
                  <strong className="validation-item-title">{issue.title}</strong>
                  <span className="validation-item-message">{issue.message}</span>
                </div>
                <button
                  className="validation-item-action"
                  onClick={() => onFocusIssue(issue)}
                  disabled={!canFocus}
                  title={canFocus ? `Review ${issue.title}.` : 'Open the relevant editor to inspect this issue.'}
                >
                  {validationActionLabel(issue)}
                </button>
              </li>
            );
          })}
        </ul>
      ) : (
        <div className="validation-ok">{emptyMessage}</div>
      )}

      {footer}
    </section>
  );
}

type OperationNoticeProps = {
  feedback: OperationFeedback;
  actions?: ReactNode;
  compact?: boolean;
};

function OperationNotice({ feedback, actions, compact = false }: OperationNoticeProps) {
  return (
    <section className={`operation-notice ${feedback.severity} ${compact ? 'compact' : ''}`}>
      <div className="operation-notice-copy">
        <span className="operation-notice-source">{feedback.source}</span>
        <strong className="operation-notice-title">{feedback.title}</strong>
        <span className="operation-notice-message">{feedback.message}</span>
      </div>
      {actions ? <div className="operation-notice-actions">{actions}</div> : null}
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
  const [snapStep, setSnapStep] = useState<number>(0.25);
  const [beatWidth, setBeatWidth] = useState<number>(42);
  const [showSectionDetails, setShowSectionDetails] = useState(false);
  const [editingChordRegionId, setEditingChordRegionId] = useState<string | null>(null);
  const [chordPickerAnchor, setChordPickerAnchor] = useState<ChordPickerAnchor | null>(null);
  const [previewSeed, setPreviewSeed] = useState(0);
  const [showUtilityTray, setShowUtilityTray] = useState(false);
  const [utilityTrayTab, setUtilityTrayTab] = useState<UtilityTrayTab>('preview');
  const [showProjectSidebar, setShowProjectSidebar] = useState(false);
  const [showSoundSidebar, setShowSoundSidebar] = useState(false);
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
  const [operationFeedback, setOperationFeedback] =
    useState<Record<OperationChannel, OperationFeedback | null>>(EMPTY_OPERATION_FEEDBACK);
  const [operationFeed, setOperationFeed] = useState<OperationFeedback[]>([]);
  const chordPickerRef = useRef<HTMLElement | null>(null);
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
  const songIssues = useMemo(() => validateSong(document), [document]);
  const draftSoundIssues = useMemo(() => validatePatch(patchDocument), [patchDocument]);
  const engineSoundIssues = useMemo<ValidationIssue[]>(
    () =>
      patchValidationOk === false
        ? [
            {
              id: 'engine:sound-check',
              scope: 'engine',
              severity: 'error',
              title: 'Engine sound check',
              message: patchValidationMessage,
              focus: { kind: 'sound' },
            },
          ]
        : [],
    [patchValidationMessage, patchValidationOk],
  );
  const soundIssues = useMemo(() => [...draftSoundIssues, ...engineSoundIssues], [draftSoundIssues, engineSoundIssues]);
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
  const selectedChordRegionRoman =
    selectedChordRegion && selectedSectionChordRegions.length > 0
      ? chordRomans[selectedSectionChordRegions.findIndex((region) => region.id === selectedChordRegion.id)] ?? selectedChordRegion.symbol
      : null;
  const chordDegreeOptions = useMemo(() => {
    const palette = buildChordPalette(chordContext.keyRoot, chordContext.mode);
    if (!selectedChordRegion || palette.some((option) => option.symbol === selectedChordRegion.symbol)) {
      return palette;
    }
    return [
      {
        id: `current-${selectedChordRegion.id}`,
        label: selectedChordRegionRoman ?? analyzeChordRoman(selectedChordRegion.symbol, chordContext.keyRoot, chordContext.mode),
        symbol: selectedChordRegion.symbol,
      },
      ...palette,
    ];
  }, [chordContext.keyRoot, chordContext.mode, selectedChordRegion, selectedChordRegionRoman]);
  const patchNodeEntries = useMemo(() => Object.entries(patchDocument.patch.nodes), [patchDocument]);
  const patchConnectionEntries = useMemo(() => Object.entries(patchDocument.patch.connections), [patchDocument]);
  const patchSignalNodeIds = useMemo(
    () =>
      patchNodeEntries
        .filter(([, node]) => node.type === 'oscillator' || node.type === 'noise')
        .map(([nodeId]) => nodeId),
    [patchNodeEntries],
  );
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
  const previewFeedback = operationFeedback.previewGeneration;
  const playbackFeedback = operationFeedback.playback;
  const soundAuditionFeedback = operationFeedback.soundAudition;
  const engineCheckFeedback = operationFeedback.engineCheck;
  const patchOperationFeedback = [soundAuditionFeedback, engineCheckFeedback].filter(
    (feedback): feedback is OperationFeedback => feedback !== null,
  );
  const selectedSectionSongIssues = useMemo(
    () =>
      selectedSection
        ? songIssues.filter(
            (issue) => issue.focus?.kind === 'song-section' && issue.focus.sectionName === selectedSection.name,
          )
        : [],
    [selectedSection, songIssues],
  );
  const canInspectCurrentSound = editorMode === 'patch' || Boolean(activePatchRole) || currentPatchAssignedRoles.length > 0;
  const totalValidationIssues = songIssues.length + soundIssues.length;
  const validationSummary = (() => {
    const parts: string[] = [];
    if (songIssues.length > 0) {
      parts.push(countLabel(songIssues.length, 'song issue'));
    }
    if (soundIssues.length > 0) {
      parts.push(countLabel(soundIssues.length, 'sound issue'));
    }
    return parts.length === 0 ? 'Checks look good' : parts.join(' · ');
  })();
  const accompanimentSummary = accompanimentLoading
    ? 'Updating preview'
    : accompanimentDirty
      ? 'Preview needs update'
      : selectedTimelineSection
        ? `${countLabel(selectedTimelineSection.notes.length, 'generated preview note')} · seed ${timelinePreview.seed ?? previewSeed}`
        : 'Preview not ready';
  const noteTraySummary = selectedNote
    ? `${midiToNoteName(selectedNote.note)} · beat ${selectedNote.beat} · dur ${selectedNote.duration}`
    : 'No note selected';
  const utilityTraySummary = utilityTrayTab === 'preview' ? accompanimentSummary : noteTraySummary;
  const footerShortcutSummary =
    editorMode === 'patch'
      ? '⌘/Ctrl+N New sound · ⌘/Ctrl+O Open sound · ⌘/Ctrl+S Save sound · ⌘/Ctrl+Enter Preview sound · ⇧⌘/Ctrl+Enter Check sound'
      : '⌘/Ctrl+N New song · ⌘/Ctrl+O Open song · ⌘/Ctrl+S Save song · ⌘/Ctrl+Enter Update preview · ⇧⌘/Ctrl+Enter Loop section · Space Play/Pause';
  const utilityWorkspaceSummary =
    utilityWorkspaceTab === 'validation'
      ? validationSummary
      : utilityWorkspaceTab === 'yaml'
        ? activeYamlView === 'song'
          ? 'Song file preview'
          : 'Sound file preview'
        : utilityWorkspaceTab === 'library'
            ? countLabel(patchLibrary.length, 'sound file')
            : operationFeed.length > 0
              ? `${countLabel(operationFeed.length, 'recent operation')} · ${transportLabel(playback)}`
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
    if (!editingChordRegionId) {
      return;
    }
    if (!selectedSectionChordRegions.some((region) => region.id === editingChordRegionId)) {
      setEditingChordRegionId(null);
      setChordPickerAnchor(null);
    }
  }, [editingChordRegionId, selectedSectionChordRegions]);

  useEffect(() => {
    if (!editingChordRegionId) {
      return;
    }

    const handlePointerDown = (event: PointerEvent) => {
      if (chordPickerRef.current?.contains(event.target as Node)) {
        return;
      }
      setEditingChordRegionId(null);
      setChordPickerAnchor(null);
    };

    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        setEditingChordRegionId(null);
        setChordPickerAnchor(null);
      }
    };

    window.addEventListener('pointerdown', handlePointerDown);
    window.addEventListener('keydown', handleKeyDown);
    return () => {
      window.removeEventListener('pointerdown', handlePointerDown);
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [editingChordRegionId]);

  useEffect(() => {
    if (!selectedNote && utilityTrayTab === 'note') {
      setUtilityTrayTab('preview');
    }
  }, [selectedNote, utilityTrayTab]);

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
    async (
      contents = yamlPreview,
      suggestedName = selectedSection?.name ?? 'preview',
      requestedSeed = previewSeed,
    ) => {
      try {
        setAccompanimentLoading(true);
        clearOperationFeedback('previewGeneration');
        const preview = await invoke<TimelinePreview>('dump_song_timeline', {
          request: {
            contents,
            suggestedName,
            previewSeed: requestedSeed,
          },
        });
        setTimelinePreview({
          ...preview,
          seed: preview.seed ?? requestedSeed,
        });
        setAccompanimentDirty(false);
        reportOperationSuccess(
          'previewGeneration',
          'Arrangement preview updated',
          `Generated a preview for ${suggestedName} with seed ${preview.seed ?? requestedSeed}.`,
          'Arrangement preview',
        );
      } catch (error) {
        setUtilityTrayTab('preview');
        setShowUtilityTray(true);
        reportOperationError(
          'previewGeneration',
          'Arrangement preview failed',
          error instanceof Error ? error.message : String(error),
          'Arrangement preview',
          'refresh-preview',
        );
      } finally {
        setAccompanimentLoading(false);
      }
    },
    [clearOperationFeedback, previewSeed, reportOperationError, reportOperationSuccess, selectedSection?.name, yamlPreview],
  );

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

  const confirmDiscardChanges = useCallback(
    (message: string, cancelMessage: string) => {
      if (typeof window === 'undefined' || typeof window.confirm !== 'function') {
        return true;
      }

      const confirmed = window.confirm(message);
      if (!confirmed) {
        setStatusMessage(cancelMessage);
      }
      return confirmed;
    },
    [setStatusMessage],
  );

  const confirmDiscardSongChanges = useCallback(
    (nextAction: string) => {
      if (!dirty) {
        return true;
      }
      return confirmDiscardChanges(
        `Discard unsaved song changes before ${nextAction}?`,
        'Kept current song changes.',
      );
    },
    [confirmDiscardChanges, dirty],
  );

  const confirmDiscardSoundChanges = useCallback(
    (nextAction: string) => {
      if (!patchDirty) {
        return true;
      }
      return confirmDiscardChanges(
        `Discard unsaved sound changes before ${nextAction}?`,
        'Kept current sound changes.',
      );
    },
    [confirmDiscardChanges, patchDirty],
  );

  function pushOperationFeedback(feedback: OperationFeedback) {
    setOperationFeed((current) => [feedback, ...current].slice(0, OPERATION_HISTORY_LIMIT));
    setOperationFeedback((current) => ({
      ...current,
      [feedback.channel]: feedback.severity === 'error' ? feedback : null,
    }));
  }

  function clearOperationFeedback(channel: OperationChannel) {
    setOperationFeedback((current) =>
      current[channel] ? { ...current, [channel]: null } : current,
    );
  }

  function reportOperationError(
    channel: OperationChannel,
    title: string,
    message: string,
    source: string,
    action: OperationAction | null,
  ) {
    pushOperationFeedback(createOperationFeedback(channel, 'error', title, message, source, action));
  }

  function reportOperationSuccess(
    channel: OperationChannel,
    title: string,
    message: string,
    source: string,
    action: OperationAction | null = null,
  ) {
    pushOperationFeedback(createOperationFeedback(channel, 'success', title, message, source, action));
  }

  const startNewSong = useCallback(() => {
    if (!confirmDiscardSongChanges('creating a new song')) {
      return false;
    }
    newDocument();
    return true;
  }, [confirmDiscardSongChanges, newDocument]);

  const openSong = useCallback(async () => {
    if (!confirmDiscardSongChanges('opening another song')) {
      return false;
    }
    try {
      const payload = await invoke<FilePayload | null>('open_song_file');
      if (!payload) {
        return false;
      }
      const parsed = parseSongText(payload.contents);
      loadDocument(parsed, payload.path);
      setTimelinePreview({ sections: [] });
      setAccompanimentDirty(true);
      clearOperationFeedback('previewGeneration');
      return true;
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
      return false;
    }
  }, [clearOperationFeedback, confirmDiscardSongChanges, loadDocument, setStatusMessage]);

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
      clearOperationFeedback('soundAudition');
      clearOperationFeedback('engineCheck');
      setStatusMessage(message);
    },
    [clearOperationFeedback, setStatusMessage],
  );

  const newPatch = useCallback(() => {
    if (!confirmDiscardSoundChanges('creating a new sound')) {
      return false;
    }
    loadPatchDocument(createDefaultPatch(), null, 'Created a new sound.');
    return true;
  }, [confirmDiscardSoundChanges, loadPatchDocument]);

  const openPatch = useCallback(async () => {
    if (!confirmDiscardSoundChanges('opening another sound')) {
      return false;
    }
    try {
      const payload = await invoke<FilePayload | null>('open_patch_file');
      if (!payload) {
        return false;
      }
      const parsed = parsePatchText(payload.contents);
      loadPatchDocument(parsed, payload.path, `Opened sound ${payload.path}`);
      return true;
    } catch (error) {
      setStatusMessage(error instanceof Error ? error.message : String(error));
      return false;
    }
  }, [confirmDiscardSoundChanges, loadPatchDocument, setStatusMessage]);

  const openPatchFromLibrary = useCallback(
    async (path: string) => {
      if (!confirmDiscardSoundChanges('opening another sound')) {
        return false;
      }
      try {
        const payload = await invoke<FilePayload>('open_patch_file_at_path', {
          request: {
            path,
          },
        });
        const parsed = parsePatchText(payload.contents);
        loadPatchDocument(parsed, payload.path, `Opened sound ${payload.path}`);
        return true;
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
        return false;
      }
    },
    [confirmDiscardSoundChanges, loadPatchDocument, setStatusMessage],
  );

  const openPatchEditorForRole = useCallback(
    async (role: PatchRole) => {
      if (!confirmDiscardSoundChanges(`loading the ${PATCH_ROLE_META[role].label} sound`)) {
        return false;
      }
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
        return true;
      }

      try {
        const payload = await invoke<FilePayload>('open_patch_file_at_path', {
          request: {
            path: assignedPath,
          },
        });
        const parsed = parsePatchText(payload.contents);
        loadPatchDocument(parsed, payload.path, `Editing the ${roleMeta.label} sound.`);
        return true;
      } catch (error) {
        setStatusMessage(error instanceof Error ? error.message : String(error));
        return false;
      }
    },
    [confirmDiscardSoundChanges, document.song.patches, enterPatchEditor, loadPatchDocument, setStatusMessage],
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

  const runContextualNew = useCallback(() => {
    if (editorMode === 'patch') {
      newPatch();
      return;
    }
    startNewSong();
  }, [editorMode, newPatch, startNewSong]);

  const runContextualOpen = useCallback(async () => {
    if (editorMode === 'patch') {
      await openPatch();
      return;
    }
    await openSong();
  }, [editorMode, openPatch, openSong]);

  const runContextualSave = useCallback(
    async (forceDialog = false) => {
      if (editorMode === 'patch') {
        await savePatch(forceDialog);
        return;
      }
      await saveSong(forceDialog);
    },
    [editorMode, savePatch, saveSong],
  );

  const runContextualUndo = useCallback(() => {
    if (editorMode === 'patch') {
      setStatusMessage('Undo is not available in the sound editor yet.');
      return;
    }
    undo();
  }, [editorMode, setStatusMessage, undo]);

  const runContextualRedo = useCallback(() => {
    if (editorMode === 'patch') {
      setStatusMessage('Redo is not available in the sound editor yet.');
      return;
    }
    redo();
  }, [editorMode, redo, setStatusMessage]);

  useEffect(() => {
    menuActionRef.current = {
      newDocument: () => {
        runContextualNew();
      },
      openSong: () => {
        void runContextualOpen();
      },
      saveSong: (forceDialog = false) => {
        void runContextualSave(forceDialog);
      },
      undo: () => runContextualUndo(),
      redo: () => runContextualRedo(),
    };
  }, [runContextualNew, runContextualOpen, runContextualRedo, runContextualSave, runContextualUndo]);

  const focusSongIssue = useCallback(
    (issue: ValidationIssue) => {
      if (editorMode === 'patch') {
        returnToArrangement();
      }
      setShowProjectSidebar(true);
      setUtilityWorkspaceTab('validation');
      setActiveYamlView('song');
      if (issue.focus?.kind === 'song-section') {
        selectSection(issue.focus.sectionName);
      }
      setStatusMessage(`Reviewing ${issue.title}.`);
    },
    [editorMode, returnToArrangement, selectSection, setStatusMessage],
  );

  const focusSoundIssue = useCallback(
    async (issue: ValidationIssue) => {
      setUtilityWorkspaceTab('validation');
      setActiveYamlView('patch');

      if (editorMode !== 'patch') {
        const roleToOpen = activePatchRole ?? currentPatchAssignedRoles[0] ?? null;
        if (!roleToOpen) {
          setStatusMessage('Open a sound from Instruments to inspect the current sound draft.');
          return;
        }
        const opened = await openPatchEditorForRole(roleToOpen);
        if (!opened) {
          return;
        }
      }

      switch (issue.focus?.kind) {
        case 'sound-node':
          setPatchWorkspaceTab('nodes');
          setSelectedPatchNodeId(issue.focus.nodeId);
          break;
        case 'sound-connection':
          setPatchWorkspaceTab('connections');
          setSelectedPatchConnectionId(issue.focus.connectionId);
          break;
        default:
          setPatchWorkspaceTab('assign');
          break;
      }

      setStatusMessage(`Reviewing ${issue.title}.`);
    },
    [activePatchRole, currentPatchAssignedRoles, editorMode, openPatchEditorForRole, setStatusMessage],
  );

  const validateCurrentPatch = useCallback(async () => {
    try {
      clearOperationFeedback('engineCheck');
      const response = await invoke<ValidatePatchResponse>('validate_patch_contents', {
        request: {
          contents: patchYamlPreview,
          suggestedName: createSuggestedPatchFileName(patchDocument.patch.name),
        },
      });
      setPatchValidationMessage(response.message);
      setPatchValidationOk(response.ok);
      if (response.ok) {
        reportOperationSuccess('engineCheck', 'Sound check passed', response.message, 'Engine check');
        setStatusMessage(response.message);
      } else {
        reportOperationError('engineCheck', 'Sound check failed', response.message, 'Engine check', 'validate-sound');
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      setPatchValidationMessage(message);
      setPatchValidationOk(false);
      reportOperationError('engineCheck', 'Sound check failed', message, 'Engine check', 'validate-sound');
    }
  }, [clearOperationFeedback, patchDocument.patch.name, patchYamlPreview, reportOperationError, reportOperationSuccess, setStatusMessage]);

  const auditionCurrentPatch = useCallback(async () => {
    if (!patchPath && patchDirty) {
      reportOperationError(
        'soundAudition',
        'Sound preview unavailable',
        'Save this sound before previewing it through the engine.',
        'Sound editor',
        null,
      );
      return;
    }
    if (!patchPath) {
      reportOperationError(
        'soundAudition',
        'Sound preview unavailable',
        'Open or save a sound file before previewing.',
        'Sound editor',
        null,
      );
      return;
    }
    try {
      clearOperationFeedback('soundAudition');
      await invoke('preview_patch_note', {
        request: {
          path: patchPath,
          midi: selectedNote?.note ?? 60,
          velocity: selectedNote?.velocity ?? 0.72,
          durationMs: 520,
        },
      });
      reportOperationSuccess('soundAudition', 'Sound preview played', `Previewed ${currentPatchLabel}.`, 'Sound editor');
      setStatusMessage(`Previewed ${currentPatchLabel}.`);
    } catch (error) {
      reportOperationError(
        'soundAudition',
        'Sound preview failed',
        error instanceof Error ? error.message : String(error),
        'Sound editor',
        'audition-sound',
      );
    }
  }, [clearOperationFeedback, currentPatchLabel, patchDirty, patchPath, reportOperationError, reportOperationSuccess, selectedNote, setStatusMessage]);

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
    const existingId = Object.entries(patchDocument.patch.connections).find(
      ([, c]) => c.from === fromNodeId && c.to === toNodeId && c.kind === kind,
    )?.[0];
    if (existingId) {
      setSelectedPatchConnectionId(existingId);
      return;
    }
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
        clearOperationFeedback('playback');
        const snapshot = await invoke<PlaybackSnapshot>('play_song_preview', {
          request: {
            contents: previewText,
            suggestedName,
            loopSectionName: loopSelectedSection && selectedSection ? selectedSection.name : null,
          },
        });
        setPlayback(snapshot);
        void refreshAccompaniment(previewText, suggestedName, previewSeed);
        reportOperationSuccess(
          'playback',
          loopSelectedSection && selectedSection ? 'Section loop started' : 'Preview playback started',
          loopSelectedSection && selectedSection
            ? `Looping ${selectedSection.name}.`
            : 'Started song preview playback.',
          'Transport',
        );
        setStatusMessage(
          loopSelectedSection && selectedSection
            ? `Looping section ${selectedSection.name}.`
            : 'Started preview playback.',
        );
      } catch (error) {
        reportOperationError(
          'playback',
          loopSelectedSection && selectedSection ? 'Section loop failed' : 'Preview playback failed',
          error instanceof Error ? error.message : String(error),
          'Transport',
          'play-preview',
        );
      }
    },
    [clearOperationFeedback, document, previewSeed, refreshAccompaniment, reportOperationError, reportOperationSuccess, selectedSection, setPlayback, setStatusMessage],
  );

  const pausePreview = useCallback(async () => {
    try {
      clearOperationFeedback('playback');
      const snapshot = await invoke<PlaybackSnapshot>('pause_song_preview');
      setPlayback(snapshot);
      reportOperationSuccess('playback', 'Preview playback paused', 'Paused preview playback.', 'Transport');
      setStatusMessage('Paused preview playback.');
    } catch (error) {
      reportOperationError(
        'playback',
        'Pause failed',
        error instanceof Error ? error.message : String(error),
        'Transport',
        'pause-preview',
      );
    }
  }, [clearOperationFeedback, reportOperationError, reportOperationSuccess, setPlayback, setStatusMessage]);

  const resumePreview = useCallback(async () => {
    try {
      clearOperationFeedback('playback');
      const snapshot = await invoke<PlaybackSnapshot>('resume_song_preview');
      setPlayback(snapshot);
      reportOperationSuccess('playback', 'Preview playback resumed', 'Resumed preview playback.', 'Transport');
      setStatusMessage('Resumed preview playback.');
    } catch (error) {
      reportOperationError(
        'playback',
        'Resume failed',
        error instanceof Error ? error.message : String(error),
        'Transport',
        'resume-preview',
      );
    }
  }, [clearOperationFeedback, reportOperationError, reportOperationSuccess, setPlayback, setStatusMessage]);

  const stopPreview = useCallback(async () => {
    try {
      clearOperationFeedback('playback');
      const snapshot = await invoke<PlaybackSnapshot>('stop_song_preview');
      setPlayback(snapshot);
      reportOperationSuccess('playback', 'Preview playback stopped', 'Stopped preview playback.', 'Transport');
      setStatusMessage('Stopped preview playback.');
    } catch (error) {
      reportOperationError(
        'playback',
        'Stop failed',
        error instanceof Error ? error.message : String(error),
        'Transport',
        'stop-preview',
      );
    }
  }, [clearOperationFeedback, reportOperationError, reportOperationSuccess, setPlayback, setStatusMessage]);

  const togglePlayback = useCallback(async () => {
    if (playback.running && playback.transportState === 'playing') {
      await pausePreview();
    } else if (playback.running) {
      await resumePreview();
    } else {
      await playPreview(false);
    }
  }, [pausePreview, playPreview, playback.running, playback.transportState, resumePreview]);

  const runContextualPrimaryAction = useCallback(async () => {
    if (editorMode === 'patch') {
      await auditionCurrentPatch();
      return;
    }
    setUtilityTrayTab('preview');
    setShowUtilityTray(true);
    await refreshAccompaniment();
  }, [auditionCurrentPatch, editorMode, refreshAccompaniment]);

  const runContextualSecondaryAction = useCallback(async () => {
    if (editorMode === 'patch') {
      await validateCurrentPatch();
      return;
    }
    setUtilityTrayTab('preview');
    setShowUtilityTray(true);
    await playPreview(Boolean(selectedSection));
  }, [editorMode, playPreview, selectedSection, validateCurrentPatch]);

  const openPlaybackDiagnostics = useCallback(() => {
    setUtilityWorkspaceTab('engine');
    if (editorMode === 'arrangement') {
      setShowProjectSidebar(true);
    }
  }, [editorMode]);

  const openPreviewChecks = useCallback(() => {
    setUtilityWorkspaceTab('validation');
    setActiveYamlView('song');
    if (editorMode === 'arrangement') {
      setShowProjectSidebar(true);
    }
  }, [editorMode]);

  const runOperationAction = useCallback(
    async (action: OperationAction) => {
      switch (action) {
        case 'refresh-preview':
          await refreshAccompaniment();
          break;
        case 'play-preview':
          await playPreview(false);
          break;
        case 'pause-preview':
          await pausePreview();
          break;
        case 'resume-preview':
          await resumePreview();
          break;
        case 'stop-preview':
          await stopPreview();
          break;
        case 'audition-sound':
          await auditionCurrentPatch();
          break;
        case 'validate-sound':
          await validateCurrentPatch();
          break;
      }
    },
    [auditionCurrentPatch, pausePreview, playPreview, refreshAccompaniment, resumePreview, stopPreview, validateCurrentPatch],
  );

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
        reportOperationError(
          'playback',
          'Note preview failed',
          error instanceof Error ? error.message : String(error),
          'Melody preview',
          null,
        );
      }
    },
    [auditionInstrument, document.song.genre, reportOperationError],
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
    if (!dirty && !patchDirty) {
      return;
    }

    const handleBeforeUnload = (event: BeforeUnloadEvent) => {
      event.preventDefault();
      event.returnValue = '';
    };

    window.addEventListener('beforeunload', handleBeforeUnload);
    return () => window.removeEventListener('beforeunload', handleBeforeUnload);
  }, [dirty, patchDirty]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      const modifier = event.metaKey || event.ctrlKey;

      if (modifier && event.code === 'KeyN') {
        event.preventDefault();
        runContextualNew();
        return;
      }

      if (modifier && event.code === 'KeyS') {
        event.preventDefault();
        void runContextualSave(event.shiftKey);
        return;
      }

      if (modifier && event.code === 'KeyO') {
        event.preventDefault();
        void runContextualOpen();
        return;
      }

      if (modifier && event.code === 'Enter') {
        if (isTextEntryTarget(event.target)) {
          return;
        }
        event.preventDefault();
        if (event.shiftKey) {
          void runContextualSecondaryAction();
        } else {
          void runContextualPrimaryAction();
        }
        return;
      }

      if (isEditableTarget(event.target)) {
        return;
      }

      if (modifier && event.code === 'KeyZ') {
        event.preventDefault();
        if (event.shiftKey) {
          runContextualRedo();
        } else {
          runContextualUndo();
        }
        return;
      }

      if (modifier && event.code === 'KeyY') {
        event.preventDefault();
        runContextualRedo();
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
    nudgeSelectedNote,
    removeSelectedNote,
    runContextualNew,
    runContextualOpen,
    runContextualPrimaryAction,
    runContextualRedo,
    runContextualSave,
    runContextualSecondaryAction,
    runContextualUndo,
    selectedNote,
    setSelectedNoteId,
    snapStep,
    togglePlayback,
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

  function addChordChange(startBeat?: number, options: { openEditor?: boolean; anchor?: ChordPickerAnchor } = {}) {
    if (!selectedSection) {
      return;
    }
    const maxStartBeat = Math.max(CHORD_SNAP_STEP, phraseBeats - CHORD_SNAP_STEP);
    const activeSpan =
      selectedSectionChordRegionSpans.find((region) => region.id === selectedChordRegion?.id) ??
      selectedSectionChordRegionSpans[selectedSectionChordRegionSpans.length - 1] ??
      null;
    const defaultStart =
      activeSpan && activeSpan.durationBeats > CHORD_SNAP_STEP
        ? quantizeBeat(activeSpan.startBeat + activeSpan.durationBeats / 2, CHORD_SNAP_STEP)
        : quantizeBeat(maxStartBeat, CHORD_SNAP_STEP);
    const resolvedStartBeat =
      startBeat == null
        ? defaultStart
        : quantizeBeat(Math.min(Math.max(CHORD_SNAP_STEP, startBeat), maxStartBeat), CHORD_SNAP_STEP);
    const occupiedStarts = new Set(selectedSectionChordRegions.slice(1).map((region) => region.startBeat));
    let nextStartBeat = resolvedStartBeat;
    while (occupiedStarts.has(nextStartBeat) && nextStartBeat < maxStartBeat) {
      nextStartBeat = quantizeBeat(nextStartBeat + CHORD_SNAP_STEP, CHORD_SNAP_STEP);
    }
    while (occupiedStarts.has(nextStartBeat) && nextStartBeat > CHORD_SNAP_STEP) {
      nextStartBeat = quantizeBeat(nextStartBeat - CHORD_SNAP_STEP, CHORD_SNAP_STEP);
    }
    if (occupiedStarts.has(nextStartBeat)) {
      setStatusMessage('No room to add another chord change at eighth-note resolution.');
      return;
    }
    const newRegion = {
      id: createChordRegionId(),
      symbol: selectedChordRegion?.symbol ?? 'Am',
      startBeat: nextStartBeat,
    } satisfies ChordRegion;
    setSelectedSectionChordRegions([...selectedSectionChordRegions, newRegion]);
    setSelectedChordRegionId(newRegion.id);
    setEditingChordRegionId(options.openEditor ? newRegion.id : null);
    setChordPickerAnchor(options.openEditor ? (options.anchor ?? null) : null);
    setStatusMessage(`Added chord change at beat ${newRegion.startBeat}.`);
  }

  function removeChordChange(regionId: string) {
    if (selectedSectionChordRegions.length <= 1) {
      return;
    }
    const currentIndex = selectedSectionChordRegions.findIndex((region) => region.id === regionId);
    if (currentIndex < 0) {
      return;
    }
    const nextRegions = selectedSectionChordRegions.filter((region) => region.id !== regionId);
    setSelectedSectionChordRegions(nextRegions);
    setSelectedChordRegionId(nextRegions[Math.max(0, currentIndex - 1)]?.id ?? nextRegions[0]?.id ?? null);
    setEditingChordRegionId((currentEditing) => {
      if (currentEditing === regionId) {
        setChordPickerAnchor(null);
        return null;
      }
      return currentEditing;
    });
    setStatusMessage('Removed chord change.');
  }

  function openChordRegionEditor(regionId: string, anchor?: ChordPickerAnchor) {
    setSelectedChordRegionId(regionId);
    setEditingChordRegionId(regionId);
    setChordPickerAnchor(anchor ?? null);
  }

  function reorderChordChange(regionId: string, targetIndex: number) {
    if (targetIndex < 0 || targetIndex >= selectedSectionChordRegions.length) {
      return;
    }
    const currentIndex = selectedSectionChordRegions.findIndex((region) => region.id === regionId);
    if (currentIndex < 0 || currentIndex === targetIndex) {
      return;
    }
    const boundaryStarts = selectedSectionChordRegions.map((region) => region.startBeat);
    const nextOrder = [...selectedSectionChordRegions];
    const [movedRegion] = nextOrder.splice(currentIndex, 1);
    if (!movedRegion) {
      return;
    }
    nextOrder.splice(targetIndex, 0, movedRegion);
    setSelectedSectionChordRegions(
      nextOrder.map((region, index) => ({
        ...region,
        startBeat: boundaryStarts[index] ?? region.startBeat,
      })),
    );
    setSelectedChordRegionId(regionId);
  }

  function moveChordBoundary(rightRegionId: string, startBeat: number) {
    const regionIndex = selectedSectionChordRegions.findIndex((region) => region.id === rightRegionId);
    if (regionIndex <= 0) {
      return;
    }
    const prevStartBeat = selectedSectionChordRegions[regionIndex - 1]?.startBeat ?? 0;
    const nextStartBeat = selectedSectionChordRegions[regionIndex + 1]?.startBeat ?? phraseBeats;
    const nextBoundary = quantizeBeat(
      clamp(
        startBeat,
        prevStartBeat + CHORD_SNAP_STEP,
        Math.max(prevStartBeat + CHORD_SNAP_STEP, nextStartBeat - CHORD_SNAP_STEP),
      ),
      CHORD_SNAP_STEP,
    );
    setSelectedSectionChordRegions(
      selectedSectionChordRegions.map((region) =>
        region.id === rightRegionId
          ? {
              ...region,
              startBeat: nextBoundary,
            }
          : region,
      ),
    );
    setSelectedChordRegionId(rightRegionId);
  }

  function selectChordDegree(symbol: string) {
    if (!selectedChordRegion) {
      return;
    }
    updateSelectedChordRegion((region) => ({
      ...region,
      symbol,
    }));
    setEditingChordRegionId(null);
    setChordPickerAnchor(null);
  }

  function resolveChordPickerStyle(anchor: ChordPickerAnchor | null): CSSProperties {
    const estimatedWidth = 150;
    const estimatedHeight = 145;
    const viewportWidth = typeof window === 'undefined' ? estimatedWidth + 32 : window.innerWidth;
    const viewportHeight = typeof window === 'undefined' ? estimatedHeight + 32 : window.innerHeight;
    const left = Math.min(Math.max(12, (anchor?.x ?? viewportWidth / 2) - 14), Math.max(12, viewportWidth - estimatedWidth - 12));
    const top = Math.min(Math.max(12, (anchor?.y ?? viewportHeight / 2) - 14), Math.max(12, viewportHeight - estimatedHeight - 12));
    return { left, top };
  }

  function updateSelectedNote(updater: (note: MelodyNote) => MelodyNote) {
    if (!selectedSection || !selectedNote) {
      return;
    }
    upsertNote(selectedSection.name, updater(selectedNote));
  }

  const generatedPreviewNotes = useMemo<OverlayNote[]>(() => {
    if (!selectedTimelineSection) {
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
  }, [selectedTimelineSection, snapStep]);

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
  const instrumentsSummary =
    patchOverrideCount === 0 ? 'Using built-in sounds' : `${countLabel(patchOverrideCount, 'custom sound')} active`;
  const soundSetupSummary =
    currentPatchAssignedRoles.length === 0
      ? `${currentPatchLabel} · not assigned`
      : `${currentPatchLabel} · ${countLabel(currentPatchAssignedRoles.length, 'role')} active`;

  const previewBeatWidth = beatWidth;

  function sanitizePreviewSeed(rawValue: number) {
    if (!Number.isFinite(rawValue)) {
      return 0;
    }
    return clamp(Math.round(rawValue), 0, MAX_PREVIEW_SEED);
  }

  function handlePreviewSeedChange(rawValue: number) {
    setPreviewSeed(sanitizePreviewSeed(rawValue));
    setAccompanimentDirty(true);
  }

  function randomizePreviewSeed() {
    const nextSeed = Math.floor(Math.random() * MAX_PREVIEW_SEED);
    setPreviewSeed(nextSeed);
    setAccompanimentDirty(true);
    void refreshAccompaniment(yamlPreview, selectedSection?.name ?? 'preview', nextSeed);
  }

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
        <div className="validation-groups">
          <ValidationIssuePanel
            title="Song file"
            subtitle={
              selectedSection && selectedSectionSongIssues.length > 0
                ? `${countLabel(selectedSectionSongIssues.length, 'issue')} in ${selectedSection.name}.`
                : 'Review sections, chords, and melody bounds before saving or previewing.'
            }
            issues={songIssues}
            emptyMessage="Song file looks good."
            onFocusIssue={focusSongIssue}
            canFocusIssue={() => true}
          />
          <ValidationIssuePanel
            title="Current sound draft"
            subtitle={
              activePatchRoleMeta
                ? `Editing the ${activePatchRoleMeta.label} sound.`
                : patchPath
                  ? `Loaded ${currentPatchLabel}.`
                  : 'This is the currently loaded sound draft.'
            }
            issues={soundIssues}
            emptyMessage="Current sound draft looks good."
            onFocusIssue={(issue) => void focusSoundIssue(issue)}
            canFocusIssue={() => canInspectCurrentSound}
            footer={
              patchValidationOk === false ? null : patchValidationMessage === 'Sound checks idle.' ? (
                <div className="hint">Run the engine check when you want to validate the current sound against the synth backend.</div>
              ) : patchValidationOk === true && soundIssues.length === 0 ? (
                <div className="validation-ok">{patchValidationMessage}</div>
              ) : (
                <div className="hint">{patchValidationMessage}</div>
              )
            }
          />
        </div>
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
        <div className="operation-log-stack">
          <section className="operation-log-panel">
            <div className="panel-heading">
              <h3>Recent operations</h3>
              <span className="panel-summary">Preview, playback, and engine actions stay here until newer activity replaces them.</span>
            </div>
            {operationFeed.length === 0 ? (
              <div className="hint">No recent operation messages yet.</div>
            ) : (
              <ul className="operation-log-list">
                {operationFeed.map((entry) => (
                  <li key={entry.id} className={`operation-log-item ${entry.severity}`}>
                    <div className="operation-log-meta">
                      <strong>{entry.title}</strong>
                      <span>{entry.source}</span>
                      <span>{formatOperationTimestamp(entry.timestamp)}</span>
                    </div>
                    <span className="operation-log-message">{entry.message}</span>
                  </li>
                ))}
              </ul>
            )}
          </section>
          <div className="log-panel">{playback.logLines.length === 0 ? 'No playback messages yet.' : playback.logLines.join('\n')}</div>
        </div>
      ) : null}
    </CollapsibleSection>
  );

  const soundAssignmentsSection = (
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
              <button
                className="instrument-row-edit"
                onClick={() => void openPatchEditorForRole(role)}
                title={`Open the full sound editor for ${roleMeta.label}.`}
              >
                Edit
              </button>
            </div>
          );
        })}
      </div>
      <div className="hint">Each choice is saved with the song. Open a sound to fine-tune it in the full sound editor.</div>
    </CollapsibleSection>
  );

  const songFlowSection = (
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
          <button
            onClick={() => void validateCurrentPatch()}
            title="Check the current sound against the engine validator. Shortcut: Shift+Cmd/Ctrl+Enter."
          >
            Check
          </button>
          <button
            onClick={() => void auditionCurrentPatch()}
            title="Play a short preview through the current sound. Shortcut: Cmd/Ctrl+Enter."
          >
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
                    <label className="checkbox-label">
                      <input
                        type="checkbox"
                        checked={selectedPatchNode.ampEnvelope !== null}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                            ampEnvelope: event.target.checked
                              ? { attack: 0.02, decay: 0.4, sustain: 0.7, release: 0.8 }
                              : null,
                          }))
                        }
                      />
                      Inline amp envelope
                    </label>
                    {selectedPatchNode.ampEnvelope
                      ? (['attack', 'decay', 'sustain', 'release'] as const).map((field) => (
                          <label key={`osc-inline-${field}`}>
                            {`Layer ${field}`}
                            <input
                              type="number"
                              step={0.01}
                              min={0}
                              max={field === 'sustain' ? 1 : undefined}
                              value={selectedPatchNode.ampEnvelope?.[field] ?? 0}
                              onChange={(event) =>
                                updatePatchNode(selectedPatchNodeId, (node) => ({
                                  ...(node as Extract<PatchNode, { type: 'oscillator' }>),
                                  ampEnvelope: {
                                    ...((node as Extract<PatchNode, { type: 'oscillator' }>).ampEnvelope ?? {
                                      attack: 0.02,
                                      decay: 0.4,
                                      sustain: 0.7,
                                      release: 0.8,
                                    }),
                                    [field]: Number(event.target.value) || 0,
                                  },
                                }))
                              }
                            />
                          </label>
                        ))
                      : null}
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
                    <label className="checkbox-label">
                      <input
                        type="checkbox"
                        checked={selectedPatchNode.ampEnvelope !== null}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'noise' }>),
                            ampEnvelope: event.target.checked
                              ? { attack: 0.02, decay: 0.4, sustain: 0.7, release: 0.8 }
                              : null,
                          }))
                        }
                      />
                      Inline amp envelope
                    </label>
                    {selectedPatchNode.ampEnvelope
                      ? (['attack', 'decay', 'sustain', 'release'] as const).map((field) => (
                          <label key={`noise-inline-${field}`}>
                            {`Layer ${field}`}
                            <input
                              type="number"
                              step={0.01}
                              min={0}
                              max={field === 'sustain' ? 1 : undefined}
                              value={selectedPatchNode.ampEnvelope?.[field] ?? 0}
                              onChange={(event) =>
                                updatePatchNode(selectedPatchNodeId, (node) => ({
                                  ...(node as Extract<PatchNode, { type: 'noise' }>),
                                  ampEnvelope: {
                                    ...((node as Extract<PatchNode, { type: 'noise' }>).ampEnvelope ?? {
                                      attack: 0.02,
                                      decay: 0.4,
                                      sustain: 0.7,
                                      release: 0.8,
                                    }),
                                    [field]: Number(event.target.value) || 0,
                                  },
                                }))
                              }
                            />
                          </label>
                        ))
                      : null}
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
                      Layer target
                      <select
                        value={selectedPatchNode.layerTarget}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'filter' }>),
                            layerTarget: event.target.value,
                          }))
                        }
                      >
                        <option value="all">all active layers</option>
                        {patchSignalNodeIds.map((nodeId) => (
                          <option key={`filter-target-${nodeId}`} value={nodeId}>
                            {nodeId}
                          </option>
                        ))}
                      </select>
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
                      LFO1 rate Hz
                      <input
                        type="number"
                        step={0.01}
                        min={0}
                        value={selectedPatchNode.lfo1RateHz}
                        onChange={(event) =>
                          updatePatchNode(selectedPatchNodeId, (node) => ({
                            ...(node as Extract<PatchNode, { type: 'output' }>),
                            lfo1RateHz: Number(event.target.value) || 0,
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
                      setEditingChordRegionId(null);
                      setChordPickerAnchor(null);
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
        <div className="workspace-toolbar-group melody-grid-controls workspace-toolbar-actions">
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

      <MelodyGrid
        beatsPerBar={activeBeatsPerBar}
        totalBeats={totalBeats}
        phraseBeats={phraseBeats}
        phraseCount={selectedSection.phraseCount}
        beatWidth={beatWidth}
        minBeatWidth={MIN_BEAT_WIDTH}
        maxBeatWidth={MAX_BEAT_WIDTH}
        quantizeStep={snapStep}
        chordQuantizeStep={CHORD_SNAP_STEP}
        notes={selectedSection.melody}
        chordRegions={selectedSectionChordRegionSpans.map((region, index) => ({
          ...region,
          romanLabel: chordRomans[index] ?? region.symbol,
        }))}
        selectedNoteId={selectedNoteId}
        selectedChordRegionId={selectedChordRegion?.id ?? null}
        onSelectNote={setSelectedNoteId}
        onSelectChordRegion={setSelectedChordRegionId}
        onCreateChordRegion={(startBeat, anchor) => addChordChange(startBeat, { openEditor: true, anchor })}
        onEditChordRegion={openChordRegionEditor}
        onDeleteChordRegion={removeChordChange}
        onReorderChordRegion={reorderChordChange}
        onResizeChordBoundary={moveChordBoundary}
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

  function renderChordPickerDialog() {
    const pickerStyle = resolveChordPickerStyle(chordPickerAnchor);
    return selectedSection && editingChordRegionId && selectedChordRegion ? (
      <section className="chord-choice-popover" ref={chordPickerRef} style={pickerStyle}>
        <div className="chord-choice-grid">
          {chordDegreeOptions.map((option) => (
            <button
              key={option.id}
              type="button"
              className={option.symbol === selectedChordRegion.symbol ? 'chord-degree-button selected' : 'chord-degree-button'}
              onClick={() => selectChordDegree(option.symbol)}
            >
              <span>{option.label}</span>
              <strong>{option.symbol}</strong>
            </button>
          ))}
        </div>
      </section>
    ) : null;
  }

  const backgroundPartsPanel = (
    <section className="generated-strip utility-tray-panel">
      <div className="strip-head">
        <div className="panel-heading">
          <h3>Arrangement preview</h3>
          <span className="panel-summary">Your melody stays on top, with the generated arrangement directly underneath for the selected section.</span>
        </div>
      </div>
      {previewFeedback ? (
        <OperationNotice
          feedback={previewFeedback}
          actions={
            <>
              {previewFeedback.action ? (
                <button onClick={() => void runOperationAction(previewFeedback.action!)}>
                  {operationActionLabel(previewFeedback.action!)}
                </button>
              ) : null}
              <button onClick={openPreviewChecks}>Open checks</button>
            </>
          }
        />
      ) : null}
      {selectedSection ? (
        <div className="preview-comparison-grid">
          <section className="preview-pane">
            <header className="preview-pane-header">
              <div>
                <h4>Written melody</h4>
                <span className="panel-summary">The notes you authored for {selectedSection.name}.</span>
              </div>
              <span className="toolbar-badge">{countLabel(selectedSection.melody.length, 'note')}</span>
            </header>
            <div className="preview-pane-grid">
              <MelodyGrid
                beatsPerBar={activeBeatsPerBar}
                totalBeats={totalBeats}
                phraseBeats={phraseBeats}
                phraseCount={selectedSection.phraseCount}
                beatWidth={previewBeatWidth}
                minBeatWidth={MIN_BEAT_WIDTH}
                maxBeatWidth={MAX_BEAT_WIDTH}
                quantizeStep={snapStep}
                chordQuantizeStep={CHORD_SNAP_STEP}
                notes={selectedSection.melody}
                chordRegions={selectedSectionChordRegionSpans.map((region, index) => ({
                  ...region,
                  romanLabel: chordRomans[index] ?? region.symbol,
                }))}
                selectedNoteId={null}
                selectedChordRegionId={null}
                onSelectNote={() => undefined}
                onSelectChordRegion={() => undefined}
                onUpsertNote={() => undefined}
                readOnly
              />
            </div>
          </section>
          <section className="preview-pane">
            <header className="preview-pane-header">
              <div>
                <h4>Generated arrangement</h4>
                <span className="panel-summary">Engine preview using seed {timelinePreview.seed ?? previewSeed}.</span>
              </div>
              {selectedTimelineSection ? (
                <span className="toolbar-badge">{countLabel(selectedTimelineSection.notes.length, 'note')}</span>
              ) : null}
            </header>
            {selectedTimelineSection ? (
              <>
                <div className="legend-list preview-legend">
                  {accompanimentLegend.map((entry) => (
                    <span key={entry.name} className="legend-chip">
                      <span className="legend-dot" style={{ background: entry.color, borderColor: entry.accentColor }} />
                      {entry.name} ({entry.count})
                    </span>
                  ))}
                </div>
                <div className="preview-pane-grid">
                  <MelodyGrid
                    beatsPerBar={selectedTimelineSection.beatsPerBar}
                    totalBeats={selectedTimelineSection.totalBeats}
                    phraseBeats={phraseBeats}
                    phraseCount={selectedSection.phraseCount}
                    beatWidth={previewBeatWidth}
                    minBeatWidth={MIN_BEAT_WIDTH}
                    maxBeatWidth={MAX_BEAT_WIDTH}
                    quantizeStep={snapStep}
                    chordQuantizeStep={CHORD_SNAP_STEP}
                    notes={[]}
                    chordRegions={selectedSectionChordRegionSpans.map((region, index) => ({
                      ...region,
                      romanLabel: chordRomans[index] ?? region.symbol,
                    }))}
                    overlayNotes={generatedPreviewNotes}
                    selectedNoteId={null}
                    selectedChordRegionId={null}
                    onSelectNote={() => undefined}
                    onSelectChordRegion={() => undefined}
                    onUpsertNote={() => undefined}
                    readOnly
                  />
                </div>
              </>
            ) : (
              <div className="preview-placeholder">
                {accompanimentLoading
                  ? 'Generating the section preview…'
                  : 'Use Update preview to generate the arrangement for this section.'}
              </div>
            )}
          </section>
        </div>
      ) : (
        <div className="hint">Select a section to compare its melody and generated arrangement.</div>
      )}
      <div className="preview-toolbar">
        <label className="preview-seed-field">
          <span>Seed</span>
          <input
            type="number"
            min={0}
            max={MAX_PREVIEW_SEED}
            step={1}
            value={previewSeed}
            onChange={(event) => handlePreviewSeedChange(Number(event.target.value))}
            disabled={accompanimentLoading}
          />
        </label>
        <button
          onClick={randomizePreviewSeed}
          disabled={!selectedSection || accompanimentLoading}
          title="Pick a fresh preview seed and regenerate the arrangement."
        >
          Randomize
        </button>
        <button
          onClick={() => void refreshAccompaniment()}
          disabled={!selectedSection || accompanimentLoading}
          title="Update the arrangement preview after melody or harmony edits. Shortcut: Cmd/Ctrl+Enter."
        >
          {accompanimentLoading ? 'Loading…' : 'Update preview'}
        </button>
        {selectedTimelineSection ? <span className="toolbar-badge">{countLabel(accompanimentLegend.length, 'part')}</span> : null}
        {accompanimentLoading ? <span className="toolbar-badge">Loading preview…</span> : null}
      </div>
    </section>
  );

  const noteInspectorPanel = selectedSection && selectedNote ? (
      <section className="note-editor utility-tray-panel">
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
  ) : (
      <section className="note-editor utility-tray-panel">
        <div className="panel-header">
          <div className="panel-heading">
            <h3>Selected note</h3>
            <span className="panel-summary">Pick a note on the grid to edit it here.</span>
          </div>
        </div>
        <div className="hint">Select a melody note to inspect its beat, duration, pitch, and velocity.</div>
      </section>
    );

  const utilityTray = (
    <section className={showUtilityTray ? 'utility-tray open' : 'utility-tray'}>
      <div className="utility-tray-bar">
        <div className="segmented-tabs utility-tray-tabs">
          <button
            className={utilityTrayTab === 'preview' ? 'toggle-button active' : 'toggle-button'}
            onClick={() => {
              setUtilityTrayTab('preview');
              setShowUtilityTray(true);
            }}
            title="Open arrangement preview tools."
          >
            Preview
          </button>
          <button
            className={utilityTrayTab === 'note' ? 'toggle-button active' : 'toggle-button'}
            onClick={() => {
              setUtilityTrayTab('note');
              setShowUtilityTray(true);
            }}
            title={selectedNote ? 'Open selected note tools.' : 'Select a note to inspect it.'}
            disabled={!selectedNote}
          >
            Selected note
          </button>
        </div>
        <span className="utility-tray-summary">{utilityTraySummary}</span>
        <button
          className="utility-tray-toggle"
          onClick={() => setShowUtilityTray((value) => !value)}
          title={showUtilityTray ? 'Collapse utility tray.' : 'Expand utility tray.'}
          aria-expanded={showUtilityTray}
        >
          <span className="utility-tray-toggle-icon" aria-hidden="true">
            ▤
          </span>
          <span className="utility-tray-toggle-chevron" aria-hidden="true">
            {showUtilityTray ? '▾' : '▸'}
          </span>
        </button>
      </div>
      {showUtilityTray ? (
        <div className="utility-tray-content">{utilityTrayTab === 'preview' ? backgroundPartsPanel : noteInspectorPanel}</div>
      ) : null}
    </section>
  );

  return (
    <div className="app-shell">
      <header className="topbar">
        <div className="toolbar-group topbar-file-group">
          {editorMode === 'arrangement' ? (
            <button
              className={showProjectSidebar ? 'topbar-sidebar-toggle active' : 'topbar-sidebar-toggle'}
              onClick={() => setShowProjectSidebar((value) => !value)}
              title={showProjectSidebar ? 'Hide project and tools.' : 'Show project and tools.'}
              aria-expanded={showProjectSidebar}
            >
              <span className="topbar-sidebar-toggle-icon" aria-hidden="true">
                ☰
              </span>
              <span className="topbar-sidebar-toggle-chevron" aria-hidden="true">
                {showProjectSidebar ? '‹' : '›'}
              </span>
              <span className="sr-only">{showProjectSidebar ? 'Hide project and tools' : 'Show project and tools'}</span>
            </button>
          ) : null}
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
          <div className="transport-stack">
            <div className="transport-controls">
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
            {playbackFeedback ? (
              <OperationNotice
                feedback={playbackFeedback}
                compact
                actions={
                  <>
                    {playbackFeedback.action ? (
                      <button onClick={() => void runOperationAction(playbackFeedback.action!)}>
                        {operationActionLabel(playbackFeedback.action!)}
                      </button>
                    ) : null}
                    <button onClick={openPlaybackDiagnostics}>Open log</button>
                  </>
                }
              />
            ) : null}
          </div>
        </div>

        <div className="toolbar-group topbar-song-meta">
          <div className="topbar-song-fields">
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
          {editorMode === 'arrangement' ? (
            <button
              className={showSoundSidebar ? 'topbar-sidebar-toggle active' : 'topbar-sidebar-toggle'}
              onClick={() => setShowSoundSidebar((value) => !value)}
              title={showSoundSidebar ? 'Hide sounds.' : 'Show sounds.'}
              aria-expanded={showSoundSidebar}
            >
              <span className="topbar-sidebar-toggle-icon" aria-hidden="true">
                ♪
              </span>
              <span className="topbar-sidebar-toggle-chevron" aria-hidden="true">
                {showSoundSidebar ? '›' : '‹'}
              </span>
              <span className="sr-only">{showSoundSidebar ? 'Hide sounds' : 'Show sounds'}</span>
            </button>
          ) : null}
        </div>
      </header>

      {editorMode === 'patch' ? (
        <div className="status-strip">
          <span>Sound editor open</span>
          <span>{transportLabel(playback)}</span>
          <span>{soundIssues.length > 0 ? `${countLabel(soundIssues.length, 'issue')} in current sound draft` : patchValidationMessage}</span>
          <span>{countLabel(patchNodeEntries.length, 'block')} · {countLabel(patchConnectionEntries.length, 'connection')}</span>
          <span>{activePatchRoleMeta ? `Editing ${activePatchRoleMeta.label}` : currentPatchLabel}</span>
        </div>
      ) : null}

      {editorMode === 'arrangement' && songIssues.length > 0 ? (
        <div className="validation-workspace-banner">
          <div className="validation-workspace-copy">
            <strong className="validation-workspace-title">Song checks need attention</strong>
            <span className="validation-workspace-message">
              {selectedSection && selectedSectionSongIssues.length > 0
                ? `${countLabel(selectedSectionSongIssues.length, 'issue')} in ${selectedSection.name}.`
                : `${countLabel(songIssues.length, 'song issue')} to review before saving or previewing.`}
            </span>
          </div>
          <div className="validation-workspace-actions">
            <button onClick={() => focusSongIssue(songIssues[0])}>Focus first issue</button>
            <button
              onClick={() => {
                setShowProjectSidebar(true);
                setUtilityWorkspaceTab('validation');
                setActiveYamlView('song');
              }}
            >
              Open checks
            </button>
          </div>
        </div>
      ) : null}

      {editorMode === 'patch' && soundIssues.length > 0 ? (
        <div className="validation-workspace-banner">
          <div className="validation-workspace-copy">
            <strong className="validation-workspace-title">Current sound draft needs attention</strong>
            <span className="validation-workspace-message">
              {countLabel(soundIssues.length, 'issue')} to review before assigning or previewing this sound.
            </span>
          </div>
          <div className="validation-workspace-actions">
            <button onClick={() => void focusSoundIssue(soundIssues[0])}>Focus first issue</button>
            <button onClick={() => {
              setUtilityWorkspaceTab('validation');
              setActiveYamlView('patch');
            }}>Open checks</button>
          </div>
        </div>
      ) : null}

      {editorMode === 'arrangement' ? (
        <main className="arrangement-workspace">
          {(showProjectSidebar || showSoundSidebar) ? (
            <button
              className="arrangement-sidebar-backdrop"
              aria-label="Close sidebars"
              onClick={() => {
                setShowProjectSidebar(false);
                setShowSoundSidebar(false);
              }}
            />
          ) : null}

          <aside className={`arrangement-side-panel left ${showProjectSidebar ? 'open' : 'closed'}`}>
            <aside className="sidebar panel arrangement-sidebar-panel arrangement-side-content">
              {songFlowSection}
              {projectToolsSection}
            </aside>
          </aside>

          <section className="arrangement-main-column">
            {melodyWorkspace}

            {utilityTray}
          </section>

          <aside className={`arrangement-side-panel right ${showSoundSidebar ? 'open' : 'closed'}`}>
            <aside className="sidebar panel arrangement-tools-panel arrangement-side-content">
              {soundAssignmentsSection}
            </aside>
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
                <button
                  onClick={() => void auditionCurrentPatch()}
                  title="Play a short preview through the current sound. Shortcut: Cmd/Ctrl+Enter."
                >
                  Preview
                </button>
                <button
                  onClick={() => void validateCurrentPatch()}
                  title="Check the current sound before saving or assigning it. Shortcut: Shift+Cmd/Ctrl+Enter."
                >
                  Check sound
                </button>
                <button onClick={() => returnToArrangement()} title="Return to the arrangement view.">
                  Back to arrangement
                </button>
              </div>
            </div>

            {patchOperationFeedback.length > 0 ? (
              <div className="operation-notice-stack">
                {patchOperationFeedback.map((feedback) => (
                  <OperationNotice
                    key={feedback.id}
                    feedback={feedback}
                    actions={
                      <>
                        {feedback.action ? (
                          <button onClick={() => void runOperationAction(feedback.action!)}>
                            {operationActionLabel(feedback.action!)}
                          </button>
                        ) : null}
                        {feedback.channel === 'engineCheck' ? (
                          <button
                            onClick={() => {
                              setUtilityWorkspaceTab('validation');
                              setActiveYamlView('patch');
                            }}
                          >
                            Open checks
                          </button>
                        ) : null}
                      </>
                    }
                  />
                ))}
              </div>
            ) : null}

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

      {renderChordPickerDialog()}

      <footer className="footer-strip">
        <span>{statusMessage}</span>
        <span>{footerShortcutSummary}</span>
      </footer>
    </div>
  );
}

export default App;
