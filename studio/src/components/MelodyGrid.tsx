import { useEffect, useMemo, useRef } from 'react';
import type { PointerEvent as ReactPointerEvent } from 'react';
import type { MelodyNote } from '../lib/song';
import { clamp, quantizeBeat } from '../lib/note';

const MIN_MIDI = 48;
const MAX_MIDI = 84;
const BEAT_WIDTH = 40;
const ROW_HEIGHT = 18;
const HEADER_WIDTH = 48;
const GRID_PADDING = 8;
const HANDLE_WIDTH = 8;

type Props = {
  beatsPerBar: number;
  totalBeats: number;
  notes: MelodyNote[];
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

function noteRect(note: MelodyNote) {
  return {
    x: HEADER_WIDTH + GRID_PADDING + note.beat * BEAT_WIDTH,
    y: GRID_PADDING + (MAX_MIDI - note.note) * ROW_HEIGHT,
    width: Math.max(HANDLE_WIDTH + 4, note.duration * BEAT_WIDTH),
    height: ROW_HEIGHT - 2,
  };
}

function pointerToBeat(clientX: number, bounds: DOMRect): number {
  const localX = clientX - bounds.left - HEADER_WIDTH - GRID_PADDING;
  return quantizeBeat(localX / BEAT_WIDTH);
}

function pointerToMidi(clientY: number, bounds: DOMRect): number {
  const localY = clientY - bounds.top - GRID_PADDING;
  const row = Math.round(localY / ROW_HEIGHT);
  return clamp(MAX_MIDI - row, MIN_MIDI, MAX_MIDI);
}

export default function MelodyGrid(props: Props) {
  const { beatsPerBar, totalBeats, notes, selectedNoteId, onSelectNote, onUpsertNote } = props;
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const hostRef = useRef<HTMLDivElement | null>(null);
  const dragStateRef = useRef<DragState>(null);

  const width = useMemo(() => HEADER_WIDTH + GRID_PADDING * 2 + totalBeats * BEAT_WIDTH, [totalBeats]);
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

    ctx.fillStyle = '#111827';
    ctx.fillRect(0, 0, width, height);

    for (let midi = MIN_MIDI; midi <= MAX_MIDI; midi += 1) {
      const y = GRID_PADDING + (MAX_MIDI - midi) * ROW_HEIGHT;
      ctx.fillStyle = midi % 12 === 0 ? '#172036' : '#141b2d';
      ctx.fillRect(0, y, width, ROW_HEIGHT - 1);
      ctx.fillStyle = '#94a3b8';
      ctx.font = '11px ui-monospace, SFMono-Regular, Menlo, monospace';
      if (midi % 12 === 0 || midi === MIN_MIDI || midi === MAX_MIDI) {
        ctx.fillText(String(midi), 8, y + 12);
      }
    }

    for (let beat = 0; beat <= totalBeats; beat += 0.25) {
      const x = HEADER_WIDTH + GRID_PADDING + beat * BEAT_WIDTH;
      const roundedBeat = Math.round(beat * 100) / 100;
      const isBeat = Number.isInteger(roundedBeat);
      const isBar = isBeat && roundedBeat % beatsPerBar === 0;
      ctx.strokeStyle = isBar ? '#334155' : isBeat ? '#243145' : '#1a2435';
      ctx.lineWidth = isBar ? 2 : 1;
      ctx.beginPath();
      ctx.moveTo(x, GRID_PADDING);
      ctx.lineTo(x, height - GRID_PADDING);
      ctx.stroke();
    }

    for (let bar = 0; bar < Math.ceil(totalBeats / beatsPerBar); bar += 1) {
      const x = HEADER_WIDTH + GRID_PADDING + bar * beatsPerBar * BEAT_WIDTH + 4;
      ctx.fillStyle = '#64748b';
      ctx.fillText(`Bar ${bar + 1}`, x, 14);
    }

    notes.forEach((note) => {
      const rect = noteRect(note);
      const selected = note.id === selectedNoteId;
      ctx.fillStyle = selected ? '#f59e0b' : '#38bdf8';
      ctx.fillRect(rect.x, rect.y, rect.width, rect.height);
      ctx.fillStyle = selected ? '#fef3c7' : '#082f49';
      ctx.fillRect(rect.x + rect.width - HANDLE_WIDTH, rect.y, HANDLE_WIDTH, rect.height);
      ctx.strokeStyle = selected ? '#fde68a' : '#7dd3fc';
      ctx.lineWidth = 1.5;
      ctx.strokeRect(rect.x + 0.5, rect.y + 0.5, rect.width - 1, rect.height - 1);
    });
  }, [beatsPerBar, height, notes, selectedNoteId, totalBeats, width]);

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
          dragState.originBeat + pointerToBeat(event.clientX, bounds) - dragState.pointerBeat,
          0,
          Math.max(0, totalBeats - note.duration),
        );
        const nextMidi = clamp(
          dragState.originMidi + pointerToMidi(event.clientY, bounds) - dragState.pointerMidi,
          MIN_MIDI,
          MAX_MIDI,
        );
        onUpsertNote({ ...note, beat: quantizeBeat(nextBeat), note: nextMidi });
      } else {
        const nextBeat = pointerToBeat(event.clientX, bounds);
        const nextDuration = clamp(
          dragState.originDuration + nextBeat - dragState.pointerBeat,
          0.25,
          Math.max(0.25, totalBeats - note.beat),
        );
        onUpsertNote({ ...note, duration: quantizeBeat(nextDuration) });
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
  }, [notes, onUpsertNote, totalBeats]);

  const handlePointerDown = (event: ReactPointerEvent<HTMLDivElement>) => {
    const bounds = hostRef.current?.getBoundingClientRect();
    if (!bounds) {
      return;
    }

    const beat = clamp(pointerToBeat(event.clientX, bounds), 0, totalBeats - 0.25);
    const midi = pointerToMidi(event.clientY, bounds);
    const hit = [...notes].reverse().find((note) => {
      const rect = noteRect(note);
      const x = event.clientX - bounds.left;
      const y = event.clientY - bounds.top;
      return x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height;
    });

    if (!hit) {
      const nextNote: MelodyNote = {
        id: `note-${Math.random().toString(36).slice(2, 10)}`,
        beat,
        note: midi,
        duration: 1,
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
    const rect = noteRect(hit);
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
      <div className="melody-grid-help">Click to add notes. Drag notes to move them. Drag the right edge to resize.</div>
      <div className="melody-grid-scroll" ref={hostRef} onPointerDown={handlePointerDown}>
        <canvas ref={canvasRef} />
      </div>
    </div>
  );
}
