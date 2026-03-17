import { useEffect, useMemo, useRef, useState } from 'react';
import type { PointerEvent as ReactPointerEvent } from 'react';
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
  notes: MelodyNote[];
  chordRegions?: ChordRegionShape[];
  overlayNotes?: OverlayNote[];
  selectedNoteId: string | null;
  selectedChordRegionId?: string | null;
  onSelectNote: (noteId: string | null) => void;
  onSelectChordRegion?: (regionId: string | null) => void;
  onUpsertChordRegion?: (region: Pick<ChordRegionShape, 'id' | 'startBeat'>) => void;
  onBeatWidthChange?: (nextBeatWidth: number) => void;
  onUpsertNote: (note: MelodyNote) => void;
  onPreviewNote?: (midi: number) => void;
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
      kind: 'move-chord';
      regionId: string;
      originStartBeat: number;
      pointerBeat: number;
    }
  | null;

function isBlackKey(midi: number): boolean {
  const pitchClass = ((midi % 12) + 12) % 12;
  return pitchClass === 1 || pitchClass === 3 || pitchClass === 6 || pitchClass === 8 || pitchClass === 10;
}

function isNearInteger(value: number): boolean {
  return Math.abs(value - Math.round(value)) < 0.0001;
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
    notes,
    chordRegions = [],
    overlayNotes = [],
    selectedNoteId,
    selectedChordRegionId = null,
    onSelectNote,
    onSelectChordRegion,
    onUpsertChordRegion,
    onBeatWidthChange,
    onUpsertNote,
    onPreviewNote,
  } =
    props;
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const hostRef = useRef<HTMLDivElement | null>(null);
  const dragStateRef = useRef<DragState>(null);
  const zoomAnchorRef = useRef<{ beat: number; localX: number } | null>(null);
  const [viewportWidth, setViewportWidth] = useState(0);
  const fitBeatWidth = useMemo(() => {
    const availableWidth = viewportWidth - HEADER_WIDTH - GRID_PADDING * 2;
    if (availableWidth <= 0 || totalBeats <= 0) {
      return beatWidth;
    }
    const rawFitWidth = availableWidth / totalBeats;
    const snappedFitWidth = Math.round(rawFitWidth / 2) * 2;
    return clamp(snappedFitWidth, minBeatWidth, maxBeatWidth);
  }, [beatWidth, maxBeatWidth, minBeatWidth, totalBeats, viewportWidth]);
  const resolvedBeatWidth = Math.max(beatWidth, fitBeatWidth);

  const displayBeats = useMemo(() => {
    const visibleBeatCount = Math.ceil(Math.max(0, viewportWidth - HEADER_WIDTH - GRID_PADDING * 2) / resolvedBeatWidth);
    return Math.max(totalBeats, visibleBeatCount);
  }, [resolvedBeatWidth, totalBeats, viewportWidth]);
  const width = useMemo(
    () => HEADER_WIDTH + GRID_PADDING * 2 + displayBeats * resolvedBeatWidth,
    [displayBeats, resolvedBeatWidth],
  );
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

      if (dragState.kind === 'move-chord') {
        const regionIndex = chordRegions.findIndex((entry) => entry.id === dragState.regionId);
        const region = regionIndex >= 0 ? chordRegions[regionIndex] : null;
        if (!region || !onUpsertChordRegion) {
          return;
        }
        const prevStartBeat = regionIndex > 0 ? chordRegions[regionIndex - 1]?.startBeat ?? 0 : 0;
        const nextStartBeat = chordRegions[regionIndex + 1]?.startBeat ?? phraseBeats;
        const nextBeat = clamp(
          dragState.originStartBeat + pointerToPhraseBeat(event.clientX, bounds, resolvedBeatWidth, quantizeStep, phraseBeats) - dragState.pointerBeat,
          prevStartBeat + quantizeStep,
          Math.max(prevStartBeat + quantizeStep, nextStartBeat - quantizeStep),
        );
        onUpsertChordRegion({ id: region.id, startBeat: quantizeBeat(nextBeat, quantizeStep) });
        return;
      }

      const note = notes.find((entry) => entry.id === dragState.noteId);
      if (!note) {
        return;
      }

      if (dragState.kind === 'move') {
        const nextBeat = clamp(
          dragState.originBeat + pointerToBeat(event.clientX, bounds, resolvedBeatWidth, quantizeStep) - dragState.pointerBeat,
          0,
          Math.max(0, totalBeats - note.duration),
        );
        const nextMidi = clamp(
          dragState.originMidi + pointerToMidi(event.clientY, bounds) - dragState.pointerMidi,
          MIN_MIDI,
          MAX_MIDI,
        );
        onUpsertNote({ ...note, beat: quantizeBeat(nextBeat, quantizeStep), note: nextMidi });
      } else {
          const nextBeat = pointerToBeat(event.clientX, bounds, resolvedBeatWidth, quantizeStep);
        const nextDuration = clamp(
          dragState.originDuration + nextBeat - dragState.pointerBeat,
          quantizeStep,
          Math.max(quantizeStep, totalBeats - note.beat),
        );
        onUpsertNote({ ...note, duration: quantizeBeat(nextDuration, quantizeStep) });
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
  }, [chordRegions, notes, onUpsertChordRegion, onUpsertNote, phraseBeats, quantizeStep, resolvedBeatWidth, totalBeats]);

  const handlePointerDown = (event: ReactPointerEvent<HTMLDivElement>) => {
    event.preventDefault();
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
      const hitRegion = [...renderedChordRegions].reverse().find((region) => {
        const regionX = HEADER_WIDTH + GRID_PADDING + region.renderStartBeat * resolvedBeatWidth;
        const regionWidth = Math.max(20, region.durationBeats * resolvedBeatWidth);
        return x >= regionX && x <= regionX + regionWidth;
      });
      onSelectNote(null);
      onSelectChordRegion?.(hitRegion?.id ?? null);
      if (hitRegion && onUpsertChordRegion && hitRegion.startBeat > 0) {
        dragStateRef.current = {
          kind: 'move-chord',
          regionId: hitRegion.id,
          originStartBeat: hitRegion.startBeat,
          pointerBeat: pointerToPhraseBeat(event.clientX, bounds, resolvedBeatWidth, quantizeStep, phraseBeats),
        };
      }
      return;
    }

    const hit = [...notes].reverse().find((note) => {
      const rect = noteRect(note, resolvedBeatWidth);
      return x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height;
    });

    if (!hit) {
      const defaultDuration = Math.max(quantizeStep, 0.25);
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

  const handleWheel = (event: ReactWheelEvent<HTMLDivElement>) => {
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

  return (
    <div className="melody-grid-shell">
      <div className="melody-grid-scroll" ref={hostRef} onPointerDown={handlePointerDown} onWheel={handleWheel}>
        <canvas ref={canvasRef} />
      </div>
    </div>
  );
}
