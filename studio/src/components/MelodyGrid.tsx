import { useEffect, useMemo, useRef, useState } from 'react';
import type { MouseEvent as ReactMouseEvent, PointerEvent as ReactPointerEvent } from 'react';
import type { WheelEvent as ReactWheelEvent } from 'react';
import { createNoteId, type MelodyNote } from '../lib/song';
import { clamp, midiToNoteName, quantizeBeat } from '../lib/note';

const MIN_MIDI = 36;
const MAX_MIDI = 96;
const ROW_HEIGHT = 18;
const HEADER_WIDTH = 64;
const GRID_PADDING = 8;
const HANDLE_WIDTH = 8;
const CHORD_LANE_HEIGHT = 28;
const CHORD_BOUNDARY_HIT_WIDTH = 6;
const FLOAT_EPSILON = 0.0001;
const MIN_NOTE_DURATION_BEATS = 0.25;

export type OverlayNote = {
  id: string;
  beat: number;
  note: number;
  duration: number;
  color: string;
  accentColor?: string;
};

type NoteShape = Pick<MelodyNote, 'beat' | 'note' | 'duration'>;

export type ChordRegionShape = {
  id: string;
  symbol: string;
  startBeat: number;
  endBeat: number;
  durationBeats: number;
  romanLabel?: string;
};

type Props = {
  beatsPerBar: number;
  totalBeats: number;
  phraseBeats: number;
  phraseCount: number;
  beatWidth: number;
  minBeatWidth?: number;
  maxBeatWidth?: number;
  quantizeStep: number;
  chordQuantizeStep?: number;
  notes: MelodyNote[];
  chordRegions?: ChordRegionShape[];
  overlayNotes?: OverlayNote[];
  selectedNoteId: string | null;
  selectedChordRegionId?: string | null;
  onSelectNote: (noteId: string | null) => void;
  onSelectChordRegion?: (regionId: string | null) => void;
  onCreateChordRegion?: (startBeat: number, anchor: { x: number; y: number }) => void;
  onEditChordRegion?: (regionId: string, anchor: { x: number; y: number }) => void;
  onDeleteChordRegion?: (regionId: string) => void;
  onReorderChordRegion?: (regionId: string, targetIndex: number) => void;
  onResizeChordBoundary?: (rightRegionId: string, startBeat: number) => void;
  onBeatWidthChange?: (nextBeatWidth: number) => void;
  onUpsertNote: (note: MelodyNote) => void;
  onPreviewNote?: (midi: number) => void;
  readOnly?: boolean;
};

type DragState =
  | {
      kind: 'move';
      noteId: string;
      originBeat: number;
      originMidi: number;
      pointerBeat: number;
      pointerMidi: number;
    }
  | {
      kind: 'resize';
      noteId: string;
      originDuration: number;
      pointerBeat: number;
    }
  | {
      kind: 'reorder-chord';
      regionId: string;
    }
  | {
      kind: 'resize-chord-boundary';
      rightRegionId: string;
    }
  | null;

type ChordContextMenuState = {
  clientX: number;
  clientY: number;
  regionId: string | null;
  startBeat: number;
} | null;

function isBlackKey(midi: number): boolean {
  const pitchClass = ((midi % 12) + 12) % 12;
  return pitchClass === 1 || pitchClass === 3 || pitchClass === 6 || pitchClass === 8 || pitchClass === 10;
}

function isNearInteger(value: number): boolean {
  return Math.abs(value - Math.round(value)) < FLOAT_EPSILON;
}

function noteRect(note: NoteShape, beatWidth: number) {
  return {
    x: HEADER_WIDTH + GRID_PADDING + note.beat * beatWidth,
    y: GRID_PADDING + CHORD_LANE_HEIGHT + (MAX_MIDI - note.note) * ROW_HEIGHT,
    width: Math.max(HANDLE_WIDTH + 4, note.duration * beatWidth),
    height: ROW_HEIGHT - 2,
  };
}

function pointerToBeat(clientX: number, bounds: DOMRect, beatWidth: number, quantizeStep: number): number {
  const localX = clientX - bounds.left - HEADER_WIDTH - GRID_PADDING;
  return quantizeBeat(localX / beatWidth, quantizeStep);
}

function pointerToMidi(clientY: number, bounds: DOMRect): number {
  const localY = clientY - bounds.top - GRID_PADDING - CHORD_LANE_HEIGHT;
  const row = Math.round(localY / ROW_HEIGHT);
  return clamp(MAX_MIDI - row, MIN_MIDI, MAX_MIDI);
}

function pointerToPhraseBeat(clientX: number, bounds: DOMRect, beatWidth: number, quantizeStep: number, phraseBeats: number): number {
  const localX = clientX - bounds.left - HEADER_WIDTH - GRID_PADDING;
  const rawBeat = ((localX / beatWidth) % phraseBeats + phraseBeats) % phraseBeats;
  return quantizeBeat(rawBeat, quantizeStep);
}

export default function MelodyGrid(props: Props) {
  const {
    beatsPerBar,
    totalBeats,
    phraseBeats,
    phraseCount,
    beatWidth,
    minBeatWidth = 24,
    maxBeatWidth = 84,
    quantizeStep,
    chordQuantizeStep = 0.5,
    notes,
    chordRegions = [],
    overlayNotes = [],
    selectedNoteId,
    selectedChordRegionId = null,
    onSelectNote,
    onSelectChordRegion,
    onCreateChordRegion,
    onEditChordRegion,
    onDeleteChordRegion,
    onReorderChordRegion,
    onResizeChordBoundary,
    onBeatWidthChange,
    onUpsertNote,
    onPreviewNote,
    readOnly = false,
  } =
    props;
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const hostRef = useRef<HTMLDivElement | null>(null);
  const contextMenuRef = useRef<HTMLDivElement | null>(null);
  const dragStateRef = useRef<DragState>(null);
  const zoomAnchorRef = useRef<{ beat: number; localX: number } | null>(null);

  const dragPropsRef = useRef({
    resolvedBeatWidth: 0,
    quantizeStep,
    chordQuantizeStep,
    phraseBeats,
    totalBeats,
    notes,
    chordRegions,
    onUpsertNote,
    onReorderChordRegion,
    onResizeChordBoundary,
  });
  // Keep ref in sync every render so drag handlers always see current values.
  Object.assign(dragPropsRef.current, {
    quantizeStep,
    chordQuantizeStep,
    phraseBeats,
    totalBeats,
    notes,
    chordRegions,
    onUpsertNote,
    onReorderChordRegion,
    onResizeChordBoundary,
  });
  const [viewportWidth, setViewportWidth] = useState(0);
  const [chordContextMenu, setChordContextMenu] = useState<ChordContextMenuState>(null);
  const fitBeatWidth = useMemo(() => {
    const availableWidth = viewportWidth - HEADER_WIDTH - GRID_PADDING * 2;
    if (availableWidth <= 0 || totalBeats <= 0) {
      return beatWidth;
    }
    const rawFitWidth = availableWidth / totalBeats;
    const snappedFitWidth = Math.max(2, Math.floor(rawFitWidth / 2) * 2);
    return clamp(snappedFitWidth, minBeatWidth, maxBeatWidth);
  }, [beatWidth, maxBeatWidth, minBeatWidth, totalBeats, viewportWidth]);
  const resolvedBeatWidth = Math.max(beatWidth, fitBeatWidth);
  dragPropsRef.current.resolvedBeatWidth = resolvedBeatWidth;

  const contentWidth = useMemo(
    () => HEADER_WIDTH + GRID_PADDING * 2 + totalBeats * resolvedBeatWidth,
    [resolvedBeatWidth, totalBeats],
  );
  const width = useMemo(() => Math.max(contentWidth, viewportWidth), [contentWidth, viewportWidth]);
  const displayBeats = useMemo(() => {
    const visibleBeatCount = Math.max(0, width - HEADER_WIDTH - GRID_PADDING * 2) / resolvedBeatWidth;
    return Math.max(totalBeats, visibleBeatCount);
  }, [resolvedBeatWidth, totalBeats, width]);
  const height = useMemo(() => GRID_PADDING * 2 + CHORD_LANE_HEIGHT + (MAX_MIDI - MIN_MIDI + 1) * ROW_HEIGHT, []);
  const renderedChordRegions = useMemo(
    () =>
      chordRegions.flatMap((region) =>
        Array.from({ length: Math.max(1, phraseCount) }, (_, phraseIndex) => ({
          ...region,
          phraseIndex,
          renderStartBeat: region.startBeat + phraseIndex * phraseBeats,
          renderEndBeat: region.endBeat + phraseIndex * phraseBeats,
        })).filter((region) => region.renderStartBeat < totalBeats),
      ),
    [chordRegions, phraseBeats, phraseCount, totalBeats],
  );

  const hitChordRegion = (localX: number) =>
    [...renderedChordRegions].reverse().find((region) => {
      const regionX = HEADER_WIDTH + GRID_PADDING + region.renderStartBeat * resolvedBeatWidth;
      const regionWidth = Math.max(20, region.durationBeats * resolvedBeatWidth);
      return localX >= regionX && localX <= regionX + regionWidth;
    });

  const hitChordBoundary = (localX: number) =>
    [...renderedChordRegions].reverse().find((region) => {
      if (region.startBeat <= 0) {
        return false;
      }
      const boundaryX = HEADER_WIDTH + GRID_PADDING + region.renderStartBeat * resolvedBeatWidth;
      return Math.abs(localX - boundaryX) <= CHORD_BOUNDARY_HIT_WIDTH;
    });

  useEffect(() => {
    const host = hostRef.current;
    if (!host || typeof ResizeObserver === 'undefined') {
      return undefined;
    }

    const observer = new ResizeObserver((entries) => {
      const nextWidth = entries[0]?.contentRect.width ?? host.clientWidth;
      setViewportWidth(nextWidth);
    });

    observer.observe(host);
    setViewportWidth(host.clientWidth);
    return () => observer.disconnect();
  }, []);

  useEffect(() => {
    if (!onBeatWidthChange || beatWidth >= fitBeatWidth - 0.5) {
      return;
    }
    onBeatWidthChange(fitBeatWidth);
  }, [beatWidth, fitBeatWidth, onBeatWidthChange]);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) {
      return;
    }

    const ratio = window.devicePixelRatio || 1;
    canvas.width = width * ratio;
    canvas.height = height * ratio;
    canvas.style.width = `${width}px`;
    canvas.style.height = `${height}px`;

    const ctx = canvas.getContext('2d');
    if (!ctx) {
      return;
    }

    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, width, height);

    ctx.fillStyle = '#070707';
    ctx.fillRect(0, 0, width, height);

    ctx.fillStyle = '#0f0f10';
    ctx.fillRect(HEADER_WIDTH, GRID_PADDING, width - HEADER_WIDTH, CHORD_LANE_HEIGHT);
    ctx.fillStyle = '#d4d4d8';
    ctx.fillRect(0, GRID_PADDING, HEADER_WIDTH - 1, CHORD_LANE_HEIGHT);
    ctx.fillStyle = '#3f3f46';
    const chordIconX = 14;
    const chordIconY = GRID_PADDING + 5;
    ctx.fillRect(chordIconX, chordIconY + 10, 22, 3);
    ctx.fillRect(chordIconX + 5, chordIconY + 4, 22, 3);
    ctx.fillRect(chordIconX + 10, chordIconY - 2, 22, 3);
    ctx.strokeStyle = '#202024';
    ctx.beginPath();
    ctx.moveTo(HEADER_WIDTH, GRID_PADDING + CHORD_LANE_HEIGHT + 0.5);
    ctx.lineTo(width, GRID_PADDING + CHORD_LANE_HEIGHT + 0.5);
    ctx.stroke();

    for (let midi = MIN_MIDI; midi <= MAX_MIDI; midi += 1) {
      const y = GRID_PADDING + CHORD_LANE_HEIGHT + (MAX_MIDI - midi) * ROW_HEIGHT;
      ctx.fillStyle = isBlackKey(midi) ? '#0d0d0d' : '#151515';
      ctx.fillRect(HEADER_WIDTH, y, width - HEADER_WIDTH, ROW_HEIGHT - 1);

      ctx.fillStyle = isBlackKey(midi) ? '#101010' : '#d4d4d8';
      ctx.fillRect(0, y, HEADER_WIDTH - 1, ROW_HEIGHT - 1);

      const noteName = midiToNoteName(midi);
      const shortName = noteName.replace(/#/, '♯');
      if (noteName.startsWith('C') || midi === MIN_MIDI || midi === MAX_MIDI) {
        ctx.fillStyle = isBlackKey(midi) ? '#a1a1aa' : '#0a0a0a';
        ctx.font = '11px ui-monospace, SFMono-Regular, Menlo, monospace';
        ctx.fillText(shortName, 10, y + 12);
      }
    }

     for (let beat = 0; beat <= displayBeats + quantizeStep / 2; beat += quantizeStep) {
        const x = HEADER_WIDTH + GRID_PADDING + beat * resolvedBeatWidth;
       const isBeat = isNearInteger(beat);
       const isBar = isBeat && Math.round(beat) % beatsPerBar === 0;
       ctx.strokeStyle = isBar ? '#4b4b52' : isBeat ? '#26262b' : '#1a1a1d';
       ctx.lineWidth = isBar ? 2 : 1;
      ctx.beginPath();
      ctx.moveTo(x, GRID_PADDING);
      ctx.lineTo(x, height - GRID_PADDING);
      ctx.stroke();
    }

    renderedChordRegions.forEach((region) => {
      const x = HEADER_WIDTH + GRID_PADDING + region.renderStartBeat * resolvedBeatWidth;
      const widthPx = Math.max(20, region.durationBeats * resolvedBeatWidth);
      const y = GRID_PADDING + 3;
      const selected = region.id === selectedChordRegionId;
      ctx.fillStyle = selected ? '#f5f5f5' : '#17171a';
      ctx.fillRect(x, y, widthPx, CHORD_LANE_HEIGHT - 6);
      ctx.strokeStyle = selected ? '#ffffff' : '#3a3a40';
      ctx.lineWidth = selected ? 1.5 : 1;
      ctx.strokeRect(x + 0.5, y + 0.5, widthPx - 1, CHORD_LANE_HEIGHT - 7);

      ctx.font = '11px ui-sans-serif, -apple-system, BlinkMacSystemFont, sans-serif';
      ctx.textBaseline = 'middle';
      ctx.fillStyle = selected ? '#111111' : '#f4f4f5';
      ctx.textAlign = 'left';
      ctx.fillText(region.symbol, x + 6, y + (CHORD_LANE_HEIGHT - 6) / 2);
      if (region.romanLabel && widthPx > 56) {
        ctx.textAlign = 'right';
        ctx.fillStyle = selected ? '#262626' : '#b9b9c0';
        ctx.fillText(region.romanLabel, x + widthPx - 6, y + (CHORD_LANE_HEIGHT - 6) / 2);
      }
    });

    for (let bar = 0; bar < Math.ceil(totalBeats / beatsPerBar); bar += 1) {
      const x = HEADER_WIDTH + GRID_PADDING + bar * beatsPerBar * resolvedBeatWidth + 6;
      ctx.fillStyle = '#a1a1aa';
      ctx.font = '11px ui-monospace, SFMono-Regular, Menlo, monospace';
      ctx.textAlign = 'left';
      ctx.fillText(`${bar + 1}`, x, 14);
    }

    overlayNotes.forEach((note) => {
      const rect = noteRect(note, resolvedBeatWidth);
      ctx.fillStyle = note.color;
      ctx.fillRect(rect.x, rect.y, rect.width, rect.height);
      ctx.strokeStyle = note.accentColor ?? note.color;
      ctx.lineWidth = 1;
      ctx.strokeRect(rect.x + 0.5, rect.y + 0.5, rect.width - 1, rect.height - 1);
    });

    notes.forEach((note) => {
      const rect = noteRect(note, resolvedBeatWidth);
      const selected = note.id === selectedNoteId;
      ctx.fillStyle = selected ? '#f5f5f5' : '#bdbdc2';
      ctx.fillRect(rect.x, rect.y, rect.width, rect.height);
      ctx.fillStyle = selected ? '#111111' : '#2a2a2e';
      ctx.fillRect(rect.x + rect.width - HANDLE_WIDTH, rect.y, HANDLE_WIDTH, rect.height);
      ctx.strokeStyle = selected ? '#ffffff' : '#ececf0';
      ctx.lineWidth = 1.5;
      ctx.strokeRect(rect.x + 0.5, rect.y + 0.5, rect.width - 1, rect.height - 1);
    });
  }, [
    beatWidth,
    beatsPerBar,
    chordRegions,
    displayBeats,
    height,
    notes,
    overlayNotes,
    quantizeStep,
    renderedChordRegions,
    resolvedBeatWidth,
    selectedChordRegionId,
    selectedNoteId,
    totalBeats,
    width,
  ]);

  useEffect(() => {
    const host = hostRef.current;
    const anchor = zoomAnchorRef.current;
    if (!host || !anchor) {
      return;
    }
    const nextScrollLeft = HEADER_WIDTH + GRID_PADDING + anchor.beat * resolvedBeatWidth - anchor.localX;
    host.scrollLeft = Math.max(0, nextScrollLeft);
    zoomAnchorRef.current = null;
  }, [resolvedBeatWidth]);

  useEffect(() => {
    const host = hostRef.current;
    if (!host) {
      return undefined;
    }

    const handlePointerMove = (event: PointerEvent) => {
      const dragState = dragStateRef.current;
      const bounds = host.getBoundingClientRect();
      if (!dragState) {
        return;
      }

      const dp = dragPropsRef.current;

      if (dragState.kind === 'resize-chord-boundary') {
        if (!dp.onResizeChordBoundary) {
          return;
        }
        const nextBeat = clamp(
          pointerToPhraseBeat(event.clientX, bounds, dp.resolvedBeatWidth, dp.chordQuantizeStep, dp.phraseBeats),
          0,
          Math.max(0, dp.phraseBeats - dp.chordQuantizeStep),
        );
        dp.onResizeChordBoundary(dragState.rightRegionId, nextBeat);
        return;
      }

      if (dragState.kind === 'reorder-chord') {
        if (!dp.onReorderChordRegion) {
          return;
        }
        const rawBeat = (event.clientX - bounds.left - HEADER_WIDTH - GRID_PADDING) / dp.resolvedBeatWidth;
        const normalizedBeat = clamp(((rawBeat % dp.phraseBeats) + dp.phraseBeats) % dp.phraseBeats, 0, Math.max(0, dp.phraseBeats - 0.001));
        const targetIndex = dp.chordRegions.findIndex((region) => normalizedBeat >= region.startBeat && normalizedBeat < region.endBeat);
        dp.onReorderChordRegion(dragState.regionId, targetIndex >= 0 ? targetIndex : dp.chordRegions.length - 1);
        return;
      }

      const note = dp.notes.find((entry) => entry.id === dragState.noteId);
      if (!note) {
        return;
      }

      if (dragState.kind === 'move') {
        const nextBeat = clamp(
          dragState.originBeat + pointerToBeat(event.clientX, bounds, dp.resolvedBeatWidth, dp.quantizeStep) - dragState.pointerBeat,
          0,
          Math.max(0, dp.totalBeats - note.duration),
        );
        const nextMidi = clamp(
          dragState.originMidi + pointerToMidi(event.clientY, bounds) - dragState.pointerMidi,
          MIN_MIDI,
          MAX_MIDI,
        );
        dp.onUpsertNote({ ...note, beat: quantizeBeat(nextBeat, dp.quantizeStep), note: nextMidi });
      } else {
        const nextBeat = pointerToBeat(event.clientX, bounds, dp.resolvedBeatWidth, dp.quantizeStep);
        const nextDuration = clamp(
          dragState.originDuration + nextBeat - dragState.pointerBeat,
          dp.quantizeStep,
          Math.max(dp.quantizeStep, dp.totalBeats - note.beat),
        );
        dp.onUpsertNote({ ...note, duration: quantizeBeat(nextDuration, dp.quantizeStep) });
      }
    };

    const handlePointerUp = () => {
      dragStateRef.current = null;
    };

    window.addEventListener('pointermove', handlePointerMove);
    window.addEventListener('pointerup', handlePointerUp);
    return () => {
      window.removeEventListener('pointermove', handlePointerMove);
      window.removeEventListener('pointerup', handlePointerUp);
    };
  }, []);

  useEffect(() => {
    if (!chordContextMenu) {
      return;
    }

    const handleGlobalPointerDown = (event: PointerEvent) => {
      if (contextMenuRef.current?.contains(event.target as Node)) {
        return;
      }
      setChordContextMenu(null);
    };

    const handleGlobalKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        setChordContextMenu(null);
      }
    };

    window.addEventListener('pointerdown', handleGlobalPointerDown);
    window.addEventListener('keydown', handleGlobalKeyDown);
    return () => {
      window.removeEventListener('pointerdown', handleGlobalPointerDown);
      window.removeEventListener('keydown', handleGlobalKeyDown);
    };
  }, [chordContextMenu]);

  const handlePointerDown = (event: ReactPointerEvent<HTMLDivElement>) => {
    if (readOnly) {
      return;
    }
    if (event.button !== 0) {
      return;
    }
    event.preventDefault();
    setChordContextMenu(null);
    const bounds = hostRef.current?.getBoundingClientRect();
    if (!bounds) {
      return;
    }

    const rawBeat = pointerToBeat(event.clientX, bounds, resolvedBeatWidth, quantizeStep);
    if (rawBeat > totalBeats) {
      onSelectNote(null);
      return;
    }
    const beat = clamp(rawBeat, 0, totalBeats - quantizeStep);
    const midi = pointerToMidi(event.clientY, bounds);
    const x = event.clientX - bounds.left;
    const y = event.clientY - bounds.top;

    if (x <= HEADER_WIDTH) {
      onSelectNote(null);
      onSelectChordRegion?.(null);
      if (y > GRID_PADDING + CHORD_LANE_HEIGHT) {
        onPreviewNote?.(midi);
      }
      return;
    }

    if (y >= GRID_PADDING && y <= GRID_PADDING + CHORD_LANE_HEIGHT) {
      const hitBoundary = hitChordBoundary(x);
      const hitRegion = hitChordRegion(x);
      onSelectNote(null);
      onSelectChordRegion?.(hitRegion?.id ?? null);
      if (hitBoundary && onResizeChordBoundary) {
        dragStateRef.current = {
          kind: 'resize-chord-boundary',
          rightRegionId: hitBoundary.id,
        };
      } else if (hitRegion && onReorderChordRegion) {
        dragStateRef.current = {
          kind: 'reorder-chord',
          regionId: hitRegion.id,
        };
      }
      return;
    }

    const hit = [...notes].reverse().find((note) => {
      const rect = noteRect(note, resolvedBeatWidth);
      return x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height;
    });

    if (!hit) {
      const defaultDuration = Math.max(quantizeStep, MIN_NOTE_DURATION_BEATS);
      const nextNote: MelodyNote = {
        id: createNoteId(),
        beat,
        note: midi,
        duration: defaultDuration,
        velocity: 0.7,
      };
      onUpsertNote(nextNote);
      onSelectNote(nextNote.id);
      dragStateRef.current = {
        kind: 'move',
        noteId: nextNote.id,
        originBeat: nextNote.beat,
        originMidi: nextNote.note,
        pointerBeat: beat,
        pointerMidi: midi,
      };
      return;
    }

    onSelectNote(hit.id);
    const rect = noteRect(hit, resolvedBeatWidth);
    dragStateRef.current =
      x >= rect.x + rect.width - HANDLE_WIDTH
        ? {
            kind: 'resize',
            noteId: hit.id,
            originDuration: hit.duration,
            pointerBeat: beat,
          }
        : {
            kind: 'move',
            noteId: hit.id,
            originBeat: hit.beat,
            originMidi: hit.note,
            pointerBeat: beat,
            pointerMidi: midi,
          };
  };

  const handleContextMenu = (event: ReactMouseEvent<HTMLDivElement>) => {
    if (readOnly) {
      setChordContextMenu(null);
      return;
    }
    const bounds = hostRef.current?.getBoundingClientRect();
    if (!bounds) {
      return;
    }

    const x = event.clientX - bounds.left;
    const y = event.clientY - bounds.top;
    const withinChordLane = x > HEADER_WIDTH && y >= GRID_PADDING && y <= GRID_PADDING + CHORD_LANE_HEIGHT;
    if (!withinChordLane) {
      setChordContextMenu(null);
      return;
    }

    event.preventDefault();
    const hitRegion = hitChordRegion(x);
    const startBeat = clamp(
      pointerToPhraseBeat(event.clientX, bounds, resolvedBeatWidth, chordQuantizeStep, phraseBeats),
      0,
      Math.max(0, phraseBeats - chordQuantizeStep),
    );
    onSelectNote(null);
    onSelectChordRegion?.(hitRegion?.id ?? null);
    setChordContextMenu({
      clientX: event.clientX + 4,
      clientY: event.clientY + 4,
      regionId: hitRegion?.id ?? null,
      startBeat,
    });
  };

  const handleWheel = (event: ReactWheelEvent<HTMLDivElement>) => {
    setChordContextMenu(null);
    const host = hostRef.current;
    if (!host) {
      return;
    }

    if (event.ctrlKey && onBeatWidthChange) {
      event.preventDefault();
      const bounds = host.getBoundingClientRect();
      const localX = event.clientX - bounds.left;
      const anchorBeat = (host.scrollLeft + localX - HEADER_WIDTH - GRID_PADDING) / resolvedBeatWidth;
      const nextBeatWidth = clamp(
        Math.round(resolvedBeatWidth * Math.exp(-event.deltaY * 0.002)),
        minBeatWidth,
        maxBeatWidth,
      );
      if (nextBeatWidth !== beatWidth) {
        zoomAnchorRef.current = { beat: Math.max(0, anchorBeat), localX };
        onBeatWidthChange(nextBeatWidth);
      }
      return;
    }

    if (event.shiftKey) {
      event.preventDefault();
      host.scrollLeft += event.deltaX !== 0 ? event.deltaX : event.deltaY;
    }
  };

  const handleCreateChordRegion = () => {
    if (!chordContextMenu || !onCreateChordRegion) {
      return;
    }
    onCreateChordRegion(chordContextMenu.startBeat, {
      x: chordContextMenu.clientX,
      y: chordContextMenu.clientY,
    });
    setChordContextMenu(null);
  };

  const handleEditChordRegion = () => {
    if (!chordContextMenu?.regionId || !onEditChordRegion) {
      return;
    }
    onEditChordRegion(chordContextMenu.regionId, {
      x: chordContextMenu.clientX,
      y: chordContextMenu.clientY,
    });
    setChordContextMenu(null);
  };

  const handleDeleteChordRegion = () => {
    if (!chordContextMenu?.regionId || !onDeleteChordRegion) {
      return;
    }
    onDeleteChordRegion(chordContextMenu.regionId);
    setChordContextMenu(null);
  };

  return (
    <div className="melody-grid-shell">
      <div
        className="melody-grid-scroll"
        ref={hostRef}
        onPointerDown={handlePointerDown}
        onContextMenu={handleContextMenu}
        onWheel={handleWheel}
      >
        <canvas ref={canvasRef} />
      </div>
      {!readOnly && chordContextMenu ? (
        <div
          className="melody-grid-context-menu"
          ref={contextMenuRef}
          style={{ left: chordContextMenu.clientX, top: chordContextMenu.clientY }}
        >
          {chordContextMenu.regionId ? (
            <button type="button" onClick={handleEditChordRegion}>
              Edit chord
            </button>
          ) : null}
          <button type="button" onClick={handleCreateChordRegion}>
            Add chord here
          </button>
          {chordContextMenu.regionId ? (
            <button type="button" onClick={handleDeleteChordRegion} disabled={chordRegions.length <= 1}>
              Delete chord
            </button>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}
