import { useEffect, useMemo, useRef } from 'react';
import type { PointerEvent as ReactPointerEvent } from 'react';
import { createNoteId, type MelodyNote } from '../lib/song';
import { clamp, midiToNoteName, quantizeBeat } from '../lib/note';

const MIN_MIDI = 36;
const MAX_MIDI = 96;
const ROW_HEIGHT = 18;
const HEADER_WIDTH = 64;
const GRID_PADDING = 8;
const HANDLE_WIDTH = 8;

export type OverlayNote = {
  id: string;
  beat: number;
  note: number;
  duration: number;
  color: string;
  accentColor?: string;
};

type NoteShape = Pick<MelodyNote, 'beat' | 'note' | 'duration'>;

type Props = {
  beatsPerBar: number;
  totalBeats: number;
  beatWidth: number;
  quantizeStep: number;
  notes: MelodyNote[];
  overlayNotes?: OverlayNote[];
  selectedNoteId: string | null;
  onSelectNote: (noteId: string | null) => void;
  onUpsertNote: (note: MelodyNote) => void;
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
    y: GRID_PADDING + (MAX_MIDI - note.note) * ROW_HEIGHT,
    width: Math.max(HANDLE_WIDTH + 4, note.duration * beatWidth),
    height: ROW_HEIGHT - 2,
  };
}

function pointerToBeat(clientX: number, bounds: DOMRect, beatWidth: number, quantizeStep: number): number {
  const localX = clientX - bounds.left - HEADER_WIDTH - GRID_PADDING;
  return quantizeBeat(localX / beatWidth, quantizeStep);
}

function pointerToMidi(clientY: number, bounds: DOMRect): number {
  const localY = clientY - bounds.top - GRID_PADDING;
  const row = Math.round(localY / ROW_HEIGHT);
  return clamp(MAX_MIDI - row, MIN_MIDI, MAX_MIDI);
}

export default function MelodyGrid(props: Props) {
  const { beatsPerBar, totalBeats, beatWidth, quantizeStep, notes, overlayNotes = [], selectedNoteId, onSelectNote, onUpsertNote } =
    props;
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const hostRef = useRef<HTMLDivElement | null>(null);
  const dragStateRef = useRef<DragState>(null);

  const width = useMemo(() => HEADER_WIDTH + GRID_PADDING * 2 + totalBeats * beatWidth, [beatWidth, totalBeats]);
  const height = useMemo(() => GRID_PADDING * 2 + (MAX_MIDI - MIN_MIDI + 1) * ROW_HEIGHT, []);

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

    ctx.fillStyle = '#0b1020';
    ctx.fillRect(0, 0, width, height);

    for (let midi = MIN_MIDI; midi <= MAX_MIDI; midi += 1) {
      const y = GRID_PADDING + (MAX_MIDI - midi) * ROW_HEIGHT;
      ctx.fillStyle = isBlackKey(midi) ? '#0c1526' : '#111827';
      ctx.fillRect(HEADER_WIDTH, y, width - HEADER_WIDTH, ROW_HEIGHT - 1);

      ctx.fillStyle = isBlackKey(midi) ? '#111827' : '#cbd5e1';
      ctx.fillRect(0, y, HEADER_WIDTH - 1, ROW_HEIGHT - 1);

      const noteName = midiToNoteName(midi);
      const shortName = noteName.replace(/#/, '♯');
      if (noteName.startsWith('C') || midi === MIN_MIDI || midi === MAX_MIDI) {
        ctx.fillStyle = isBlackKey(midi) ? '#94a3b8' : '#0f172a';
        ctx.font = '11px ui-monospace, SFMono-Regular, Menlo, monospace';
        ctx.fillText(shortName, 10, y + 12);
      }
    }

    for (let beat = 0; beat <= totalBeats + quantizeStep / 2; beat += quantizeStep) {
      const x = HEADER_WIDTH + GRID_PADDING + beat * beatWidth;
      const isBeat = isNearInteger(beat);
      const isBar = isBeat && Math.round(beat) % beatsPerBar === 0;
      ctx.strokeStyle = isBar ? '#334155' : isBeat ? '#243145' : '#182131';
      ctx.lineWidth = isBar ? 2 : 1;
      ctx.beginPath();
      ctx.moveTo(x, GRID_PADDING);
      ctx.lineTo(x, height - GRID_PADDING);
      ctx.stroke();
    }

    for (let bar = 0; bar < Math.ceil(totalBeats / beatsPerBar); bar += 1) {
      const x = HEADER_WIDTH + GRID_PADDING + bar * beatsPerBar * beatWidth + 6;
      ctx.fillStyle = '#64748b';
      ctx.font = '11px ui-monospace, SFMono-Regular, Menlo, monospace';
      ctx.fillText(`${bar + 1}`, x, 14);
    }

    overlayNotes.forEach((note) => {
      const rect = noteRect(note, beatWidth);
      ctx.fillStyle = note.color;
      ctx.fillRect(rect.x, rect.y, rect.width, rect.height);
      ctx.strokeStyle = note.accentColor ?? note.color;
      ctx.lineWidth = 1;
      ctx.strokeRect(rect.x + 0.5, rect.y + 0.5, rect.width - 1, rect.height - 1);
    });

    notes.forEach((note) => {
      const rect = noteRect(note, beatWidth);
      const selected = note.id === selectedNoteId;
      ctx.fillStyle = selected ? '#f59e0b' : '#38bdf8';
      ctx.fillRect(rect.x, rect.y, rect.width, rect.height);
      ctx.fillStyle = selected ? '#fef3c7' : '#082f49';
      ctx.fillRect(rect.x + rect.width - HANDLE_WIDTH, rect.y, HANDLE_WIDTH, rect.height);
      ctx.strokeStyle = selected ? '#fde68a' : '#7dd3fc';
      ctx.lineWidth = 1.5;
      ctx.strokeRect(rect.x + 0.5, rect.y + 0.5, rect.width - 1, rect.height - 1);
    });
  }, [beatWidth, beatsPerBar, height, notes, overlayNotes, quantizeStep, selectedNoteId, totalBeats, width]);

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

      const note = notes.find((entry) => entry.id === dragState.noteId);
      if (!note) {
        return;
      }

      if (dragState.kind === 'move') {
        const nextBeat = clamp(
          dragState.originBeat + pointerToBeat(event.clientX, bounds, beatWidth, quantizeStep) - dragState.pointerBeat,
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
        const nextBeat = pointerToBeat(event.clientX, bounds, beatWidth, quantizeStep);
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
  }, [beatWidth, notes, onUpsertNote, quantizeStep, totalBeats]);

  const handlePointerDown = (event: ReactPointerEvent<HTMLDivElement>) => {
    event.preventDefault();
    const bounds = hostRef.current?.getBoundingClientRect();
    if (!bounds) {
      return;
    }

    const beat = clamp(pointerToBeat(event.clientX, bounds, beatWidth, quantizeStep), 0, totalBeats - quantizeStep);
    const midi = pointerToMidi(event.clientY, bounds);
    const hit = [...notes].reverse().find((note) => {
      const rect = noteRect(note, beatWidth);
      const x = event.clientX - bounds.left;
      const y = event.clientY - bounds.top;
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
    const rect = noteRect(hit, beatWidth);
    const x = event.clientX - bounds.left;
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

  return (
    <div className="melody-grid-shell">
      <div className="melody-grid-scroll" ref={hostRef} onPointerDown={handlePointerDown}>
        <canvas ref={canvasRef} />
      </div>
    </div>
  );
}
