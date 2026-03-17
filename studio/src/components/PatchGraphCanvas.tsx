import {
  useEffect,
  useMemo,
  useRef,
  useState,
  type PointerEvent as ReactPointerEvent,
  type SyntheticEvent,
} from 'react';
import type { PatchConnection, PatchConnectionKind, PatchDocument, PatchNode } from '../lib/patch';

type GraphToolMode = 'select' | 'wire';

type NodePosition = {
  x: number;
  y: number;
};

type DragState = {
  nodeId: string;
  pointerOffsetX: number;
  pointerOffsetY: number;
} | null;

type WireDraft = {
  fromNodeId: string;
  pointerX: number;
  pointerY: number;
  targetNodeId: string | null;
} | null;

type PatchGraphCanvasProps = {
  patchDocument: PatchDocument;
  selectedNodeId: string | null;
  selectedConnectionId: string | null;
  toolMode: GraphToolMode;
  connectionKind: PatchConnectionKind;
  onSelectNode: (nodeId: string) => void;
  onSelectConnection: (connectionId: string) => void;
  onCreateConnection: (fromNodeId: string, toNodeId: string, kind: PatchConnectionKind) => void;
  onUpdateNode: (nodeId: string, updater: (node: PatchNode) => PatchNode) => void;
  onUpdateConnection: (connectionId: string, updater: (connection: PatchConnection) => PatchConnection) => void;
  onRemoveConnection: (connectionId: string) => void;
};

const NODE_WIDTH = 236;
const NODE_HEIGHT = 232;
const LANE_X = {
  source: 64,
  envelope: 320,
  filter: 596,
  output: 872,
  misc: 596,
} as const;

const CONNECTION_KIND_OPTIONS: { value: PatchConnectionKind; label: string }[] = [
  { value: 'signal', label: 'Audio' },
  { value: 'amp_envelope', label: 'Volume shape' },
  { value: 'filter_envelope', label: 'Tone shape' },
  { value: 'hard_sync', label: 'Hard sync' },
];

const NODE_BADGES: Record<PatchNode['type'], string> = {
  oscillator: 'OSC',
  noise: 'NOI',
  envelope: 'ENV',
  filter: 'FLT',
  output: 'OUT',
};

function isSourceNode(node: PatchNode): boolean {
  return node.type === 'oscillator' || node.type === 'noise';
}

function isEnvelopeNode(node: PatchNode): boolean {
  return node.type === 'envelope';
}

function isFilterNode(node: PatchNode): boolean {
  return node.type === 'filter';
}

function isOutputNode(node: PatchNode): boolean {
  return node.type === 'output';
}

function canStartWire(node: PatchNode): boolean {
  return !isOutputNode(node);
}

function canReceiveWire(node: PatchNode): boolean {
  return !isEnvelopeNode(node);
}

function connectionKindLabel(kind: PatchConnectionKind): string {
  return CONNECTION_KIND_OPTIONS.find((option) => option.value === kind)?.label ?? kind.replace('_', ' ');
}

function buildInitialLayout(document: PatchDocument): Record<string, NodePosition> {
  const sources: string[] = [];
  const envelopes: string[] = [];
  const filters: string[] = [];
  const outputs: string[] = [];
  const misc: string[] = [];

  for (const [nodeId, node] of Object.entries(document.patch.nodes)) {
    if (isSourceNode(node)) {
      sources.push(nodeId);
    } else if (isEnvelopeNode(node)) {
      envelopes.push(nodeId);
    } else if (isFilterNode(node)) {
      filters.push(nodeId);
    } else if (isOutputNode(node)) {
      outputs.push(nodeId);
    } else {
      misc.push(nodeId);
    }
  }

  const positions: Record<string, NodePosition> = {};
  const placeLane = (ids: string[], x: number, startY: number, gap = 254) => {
    ids.forEach((nodeId, index) => {
      positions[nodeId] = {
        x,
        y: startY + index * gap,
      };
    });
  };

  placeLane(sources, LANE_X.source, 72);
  placeLane(envelopes, LANE_X.envelope, 56);
  placeLane(filters, LANE_X.filter, 134);
  placeLane(outputs, LANE_X.output, 190);
  placeLane(misc, LANE_X.misc, 360);

  return positions;
}

function mergeLayout(document: PatchDocument, previous: Record<string, NodePosition>): Record<string, NodePosition> {
  const next: Record<string, NodePosition> = {};
  const defaults = buildInitialLayout(document);
  for (const nodeId of Object.keys(document.patch.nodes)) {
    next[nodeId] = previous[nodeId] ?? defaults[nodeId] ?? { x: 64, y: 64 };
  }
  return next;
}

function outputSocketPoint(position: NodePosition): NodePosition {
  return {
    x: position.x + NODE_WIDTH,
    y: position.y + NODE_HEIGHT / 2,
  };
}

function inputSocketPoint(position: NodePosition): NodePosition {
  return {
    x: position.x,
    y: position.y + NODE_HEIGHT / 2,
  };
}

function bezierPath(start: NodePosition, end: NodePosition): string {
  const curve = Math.max(72, Math.abs(end.x - start.x) * 0.5);
  return `M ${start.x} ${start.y} C ${start.x + curve} ${start.y}, ${end.x - curve} ${end.y}, ${end.x} ${end.y}`;
}

function connectionPath(from: NodePosition, to: NodePosition): string {
  return bezierPath(outputSocketPoint(from), inputSocketPoint(to));
}

function readInputSocketNodeId(clientX: number, clientY: number): string | null {
  if (typeof document === 'undefined') {
    return null;
  }
  const element = document.elementFromPoint(clientX, clientY);
  if (!(element instanceof Element)) {
    return null;
  }
  const socket = element.closest<HTMLElement>('[data-patch-input-node-id]');
  return socket?.dataset.patchInputNodeId ?? null;
}

function connectionEditorAnchor(
  from: NodePosition,
  to: NodePosition,
  surfaceWidth: number,
  surfaceHeight: number,
): NodePosition {
  const start = outputSocketPoint(from);
  const end = inputSocketPoint(to);
  const width = 236;
  const height = 168;
  const x = Math.min(surfaceWidth - width - 18, Math.max(18, (start.x + end.x) * 0.5 - width * 0.5));
  const y = Math.min(surfaceHeight - height - 18, Math.max(18, (start.y + end.y) * 0.5 - height * 0.5));
  return { x, y };
}

function PatchGraphCanvas({
  patchDocument,
  selectedNodeId,
  selectedConnectionId,
  toolMode,
  connectionKind,
  onSelectNode,
  onSelectConnection,
  onCreateConnection,
  onUpdateNode,
  onUpdateConnection,
  onRemoveConnection,
}: PatchGraphCanvasProps) {
  const [positions, setPositions] = useState<Record<string, NodePosition>>(() => buildInitialLayout(patchDocument));
  const [wireDraft, setWireDraft] = useState<WireDraft>(null);
  const dragStateRef = useRef<DragState>(null);
  const wireDraftRef = useRef<WireDraft>(null);
  const shellRef = useRef<HTMLDivElement | null>(null);

  const selectedConnection =
    selectedConnectionId && patchDocument.patch.connections[selectedConnectionId]
      ? patchDocument.patch.connections[selectedConnectionId]
      : null;

  function setWireDraftState(next: WireDraft) {
    wireDraftRef.current = next;
    setWireDraft(next);
  }

  const resolveWireTargetNodeId = (clientX: number, clientY: number, sourceNodeId: string): string | null => {
    const nodeId = readInputSocketNodeId(clientX, clientY);
    if (!nodeId || nodeId === sourceNodeId) {
      return null;
    }
    const node = patchDocument.patch.nodes[nodeId];
    return node && canReceiveWire(node) ? nodeId : null;
  };

  useEffect(() => {
    setPositions((current) => mergeLayout(patchDocument, current));
  }, [patchDocument]);

  useEffect(() => {
    if (toolMode !== 'wire') {
      setWireDraftState(null);
    }
  }, [toolMode]);

  useEffect(() => {
    if (!wireDraftRef.current) {
      return;
    }
    if (!patchDocument.patch.nodes[wireDraftRef.current.fromNodeId]) {
      setWireDraftState(null);
    }
  }, [patchDocument]);

  useEffect(() => {
    const handlePointerMove = (event: PointerEvent) => {
      const dragState = dragStateRef.current;
      const shell = shellRef.current;
      if (dragState && shell) {
        const rect = shell.getBoundingClientRect();
        const nextX = Math.max(16, event.clientX - rect.left - dragState.pointerOffsetX);
        const nextY = Math.max(16, event.clientY - rect.top - dragState.pointerOffsetY);
        setPositions((current) => ({
          ...current,
          [dragState.nodeId]: {
            x: nextX,
            y: nextY,
          },
        }));
      }

      const currentWireDraft = wireDraftRef.current;
      if (!currentWireDraft || !shell) {
        return;
      }

      const rect = shell.getBoundingClientRect();
      setWireDraftState({
        ...currentWireDraft,
        pointerX: Math.max(0, event.clientX - rect.left),
        pointerY: Math.max(0, event.clientY - rect.top),
        targetNodeId: resolveWireTargetNodeId(event.clientX, event.clientY, currentWireDraft.fromNodeId),
      });
    };

    const handlePointerUp = (event: PointerEvent) => {
      dragStateRef.current = null;

      const currentWireDraft = wireDraftRef.current;
      if (!currentWireDraft) {
        return;
      }

      const targetNodeId =
        resolveWireTargetNodeId(event.clientX, event.clientY, currentWireDraft.fromNodeId) ??
        currentWireDraft.targetNodeId;
      if (targetNodeId && targetNodeId !== currentWireDraft.fromNodeId) {
        onCreateConnection(currentWireDraft.fromNodeId, targetNodeId, connectionKind);
      }
      setWireDraftState(null);
    };

    window.addEventListener('pointermove', handlePointerMove);
    window.addEventListener('pointerup', handlePointerUp);
    return () => {
      window.removeEventListener('pointermove', handlePointerMove);
      window.removeEventListener('pointerup', handlePointerUp);
    };
  }, [connectionKind, onCreateConnection, patchDocument]);

  const canvasSize = useMemo(() => {
    const values = Object.values(positions);
    if (values.length === 0) {
      return { width: 1080, height: 520 };
    }

    const maxX = Math.max(...values.map((position) => position.x + NODE_WIDTH + 80));
    const maxY = Math.max(...values.map((position) => position.y + NODE_HEIGHT + 80));
    return {
      width: Math.max(1080, maxX),
      height: Math.max(520, maxY),
    };
  }, [positions]);

  const selectedConnectionAnchor = useMemo(() => {
    if (!selectedConnection) {
      return null;
    }
    const from = positions[selectedConnection.from];
    const to = positions[selectedConnection.to];
    if (!from || !to) {
      return null;
    }
    return connectionEditorAnchor(from, to, canvasSize.width, canvasSize.height);
  }, [canvasSize.height, canvasSize.width, positions, selectedConnection]);

  const wireSourcePosition = wireDraft ? positions[wireDraft.fromNodeId] ?? null : null;
  const wireTargetPosition =
    wireDraft && wireDraft.targetNodeId ? positions[wireDraft.targetNodeId] ?? null : null;

  function beginDrag(nodeId: string, event: ReactPointerEvent<HTMLDivElement>) {
    if (toolMode !== 'select' || event.button !== 0) {
      return;
    }

    const position = positions[nodeId];
    if (!position) {
      return;
    }

    const shell = shellRef.current;
    if (!shell) {
      return;
    }

    const rect = shell.getBoundingClientRect();
    dragStateRef.current = {
      nodeId,
      pointerOffsetX: event.clientX - rect.left - position.x,
      pointerOffsetY: event.clientY - rect.top - position.y,
    };
  }

  function beginWireDrag(nodeId: string, event: ReactPointerEvent<HTMLButtonElement>) {
    const node = patchDocument.patch.nodes[nodeId];
    if (!node) {
      return;
    }

    onSelectNode(nodeId);

    if (toolMode !== 'wire' || event.button !== 0 || !canStartWire(node)) {
      return;
    }

    const shell = shellRef.current;
    if (!shell) {
      return;
    }

    event.preventDefault();
    event.stopPropagation();

    const rect = shell.getBoundingClientRect();
    setWireDraftState({
      fromNodeId: nodeId,
      pointerX: Math.max(0, event.clientX - rect.left),
      pointerY: Math.max(0, event.clientY - rect.top),
      targetNodeId: null,
    });
    onSelectConnection('');
  }

  function stopCanvasEvent(event: SyntheticEvent) {
    event.stopPropagation();
  }

  function renderNodeControls(nodeId: string, node: PatchNode) {
    switch (node.type) {
      case 'oscillator':
        return (
          <>
            <label className="patch-graph-control">
              <span>Wave</span>
              <select
                value={node.waveform}
                onClick={stopCanvasEvent}
                onChange={(event) =>
                  onUpdateNode(nodeId, (current) => ({
                    ...(current as Extract<PatchNode, { type: 'oscillator' }>),
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
            <label className="patch-graph-control">
              <span>Level</span>
              <div className="patch-graph-control-row">
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={0}
                  max={1.2}
                  step={0.01}
                  value={node.level}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'oscillator' }>),
                      level: Number(event.target.value) || 0,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0}
                  step={0.01}
                  value={node.level}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'oscillator' }>),
                      level: Number(event.target.value) || 0,
                    }))
                  }
                />
              </div>
            </label>
            <div className="patch-graph-mini-grid">
              <label className="patch-graph-control">
                <span>Oct</span>
                <input
                  className="patch-graph-number"
                  type="number"
                  step={1}
                  value={node.pitch.octaves}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'oscillator' }>),
                      pitch: {
                        ...(current as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                        octaves: Math.round(Number(event.target.value) || 0),
                      },
                    }))
                  }
                />
              </label>
              <label className="patch-graph-control">
                <span>Cent</span>
                <input
                  className="patch-graph-number"
                  type="number"
                  step={1}
                  value={node.pitch.cents}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'oscillator' }>),
                      pitch: {
                        ...(current as Extract<PatchNode, { type: 'oscillator' }>).pitch,
                        cents: Number(event.target.value) || 0,
                      },
                    }))
                  }
                />
              </label>
            </div>
          </>
        );
      case 'noise':
        return (
          <>
            <label className="patch-graph-control">
              <span>Color</span>
              <select
                value={node.color}
                onClick={stopCanvasEvent}
                onChange={(event) =>
                  onUpdateNode(nodeId, (current) => ({
                    ...(current as Extract<PatchNode, { type: 'noise' }>),
                    color: event.target.value as Extract<PatchNode, { type: 'noise' }>['color'],
                  }))
                }
              >
                <option value="white">white</option>
                <option value="pink">pink</option>
                <option value="mix">mix</option>
              </select>
            </label>
            <label className="patch-graph-control">
              <span>Level</span>
              <div className="patch-graph-control-row">
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={0}
                  max={1}
                  step={0.01}
                  value={node.level}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'noise' }>),
                      level: Number(event.target.value) || 0,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0}
                  step={0.01}
                  value={node.level}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'noise' }>),
                      level: Number(event.target.value) || 0,
                    }))
                  }
                />
              </div>
            </label>
            <label className="patch-graph-control">
              <span>Mix</span>
              <div className="patch-graph-control-row">
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={0}
                  max={1}
                  step={0.01}
                  value={node.mix}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'noise' }>),
                      mix: Number(event.target.value) || 0,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0}
                  max={1}
                  step={0.01}
                  value={node.mix}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'noise' }>),
                      mix: Number(event.target.value) || 0,
                    }))
                  }
                />
              </div>
            </label>
          </>
        );
      case 'envelope':
        return (
          <div className="patch-graph-envelope-grid">
            {(['attack', 'decay', 'sustain', 'release'] as const).map((field) => (
              <label key={field} className="patch-graph-control">
                <span>{field[0].toUpperCase()}</span>
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={0}
                  max={field === 'sustain' ? 1 : 4}
                  step={0.01}
                  value={node[field]}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'envelope' }>),
                      [field]: Number(event.target.value) || 0,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0}
                  max={field === 'sustain' ? 1 : undefined}
                  step={0.01}
                  value={node[field]}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'envelope' }>),
                      [field]: Number(event.target.value) || 0,
                    }))
                  }
                />
              </label>
            ))}
          </div>
        );
      case 'filter':
        return (
          <>
            <div className="patch-graph-mini-grid">
              <label className="patch-graph-control">
                <span>Mode</span>
                <select
                  value={node.mode}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
                      mode: event.target.value as Extract<PatchNode, { type: 'filter' }>['mode'],
                    }))
                  }
                >
                  <option value="lowpass">LP</option>
                  <option value="highpass">HP</option>
                  <option value="bandpass">BP</option>
                </select>
              </label>
              <label className="patch-graph-control">
                <span>Slope</span>
                <select
                  value={node.slope}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
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
            </div>
            <label className="patch-graph-control">
              <span>Cutoff</span>
              <div className="patch-graph-control-row">
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={50}
                  max={12000}
                  step={1}
                  value={node.cutoffHz}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
                      cutoffHz: Number(event.target.value) || 50,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={1}
                  step={1}
                  value={node.cutoffHz}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
                      cutoffHz: Number(event.target.value) || 1,
                    }))
                  }
                />
              </div>
            </label>
            <div className="patch-graph-mini-grid">
              <label className="patch-graph-control">
                <span>Q</span>
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0.01}
                  step={0.01}
                  value={node.q}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
                      q: Number(event.target.value) || 0.01,
                    }))
                  }
                />
              </label>
              <label className="patch-graph-control">
                <span>Env</span>
                <input
                  className="patch-graph-number"
                  type="number"
                  step={0.01}
                  value={node.envAmountOct}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'filter' }>),
                      envAmountOct: Number(event.target.value) || 0,
                    }))
                  }
                />
              </label>
            </div>
          </>
        );
      case 'output':
        return (
          <>
            <label className="patch-graph-control">
              <span>Gain</span>
              <div className="patch-graph-control-row">
                <input
                  className="patch-graph-slider"
                  type="range"
                  min={0}
                  max={1.4}
                  step={0.01}
                  value={node.gain}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'output' }>),
                      gain: Number(event.target.value) || 0,
                    }))
                  }
                />
                <input
                  className="patch-graph-number"
                  type="number"
                  min={0}
                  step={0.01}
                  value={node.gain}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'output' }>),
                      gain: Number(event.target.value) || 0,
                    }))
                  }
                />
              </div>
            </label>
            <div className="patch-graph-mini-grid">
              <label className="patch-graph-control">
                <span>Mode</span>
                <select
                  value={node.playMode}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'output' }>),
                      playMode: event.target.value as Extract<PatchNode, { type: 'output' }>['playMode'],
                    }))
                  }
                >
                  <option value="poly">poly</option>
                  <option value="monolegato">mono</option>
                </select>
              </label>
              <label className="patch-graph-control">
                <span>Poly</span>
                <input
                  className="patch-graph-number"
                  type="number"
                  min={1}
                  step={1}
                  value={node.polyMax}
                  onClick={stopCanvasEvent}
                  onChange={(event) =>
                    onUpdateNode(nodeId, (current) => ({
                      ...(current as Extract<PatchNode, { type: 'output' }>),
                      polyMax: Math.round(Number(event.target.value) || 1),
                    }))
                  }
                />
              </label>
            </div>
          </>
        );
    }
  }

  return (
    <div className="patch-graph-shell" ref={shellRef}>
      <div className="patch-graph-hint-row">
        <div className="patch-graph-status">
          <span className="toolbar-badge">{toolMode === 'wire' ? 'Connect mode' : 'Move & select'}</span>
          <span className="toolbar-badge">{connectionKindLabel(connectionKind)}</span>
          <span className="toolbar-badge">{Object.keys(patchDocument.patch.nodes).length} blocks</span>
        </div>
        <span className="hint">
          {wireDraft
            ? wireDraft.targetNodeId
              ? `Drop to connect ${wireDraft.fromNodeId} → ${wireDraft.targetNodeId} as ${connectionKindLabel(connectionKind)}.`
              : `Dragging from ${wireDraft.fromNodeId}. Hover an input socket, then release to add ${connectionKindLabel(connectionKind)}.`
            : toolMode === 'wire'
              ? `Connect mode is active. Drag from an output socket to an input socket to add ${connectionKindLabel(connectionKind)}.`
              : selectedConnectionId
                ? `Selected ${selectedConnectionId}. Edit this connection on the canvas or in the routing panel.`
                : 'Move & select mode is active. Drag blocks by the header to arrange the layout.'}
        </span>
      </div>

      <div className="patch-graph-scroll">
        <div
          className="patch-graph-surface"
          style={{ width: canvasSize.width, height: canvasSize.height }}
          onClick={() => {
            if (wireDraft) {
              setWireDraftState(null);
            }
          }}
        >
          <svg className="patch-graph-svg" width={canvasSize.width} height={canvasSize.height}>
            {Object.entries(patchDocument.patch.connections).map(([connectionId, connection]) => {
              const from = positions[connection.from];
              const to = positions[connection.to];
              if (!from || !to) {
                return null;
              }

              return (
                <path
                  key={connectionId}
                  d={connectionPath(from, to)}
                  className={connectionId === selectedConnectionId ? 'patch-graph-wire selected' : 'patch-graph-wire'}
                  onClick={(event) => {
                    event.stopPropagation();
                    onSelectConnection(connectionId);
                  }}
                >
                  <title>{`${connectionKindLabel(connection.kind)}: ${connection.from} → ${connection.to}`}</title>
                </path>
              );
            })}

            {wireDraft && wireSourcePosition ? (
              <>
                <path
                  d={bezierPath(
                    outputSocketPoint(wireSourcePosition),
                    wireTargetPosition ? inputSocketPoint(wireTargetPosition) : { x: wireDraft.pointerX, y: wireDraft.pointerY },
                  )}
                  className={
                    wireDraft.targetNodeId ? 'patch-graph-wire preview armed' : 'patch-graph-wire preview'
                  }
                />
                <circle
                  className={wireDraft.targetNodeId ? 'patch-graph-preview-dot armed' : 'patch-graph-preview-dot'}
                  cx={wireTargetPosition ? inputSocketPoint(wireTargetPosition).x : wireDraft.pointerX}
                  cy={wireTargetPosition ? inputSocketPoint(wireTargetPosition).y : wireDraft.pointerY}
                  r="5"
                />
              </>
            ) : null}
          </svg>

          {selectedConnection && selectedConnectionAnchor ? (
            <div
              className="patch-graph-connection-editor"
              style={{ left: selectedConnectionAnchor.x, top: selectedConnectionAnchor.y }}
              onClick={stopCanvasEvent}
              onPointerDown={stopCanvasEvent}
            >
              <div className="patch-graph-connection-header">
                <strong>{selectedConnectionId}</strong>
                <span>
                  {selectedConnection.from} → {selectedConnection.to}
                </span>
              </div>
              <div className="patch-graph-connection-grid">
                <label className="patch-graph-control">
                  <span>Source</span>
                  <select
                    value={selectedConnection.from}
                    onChange={(event) =>
                      onUpdateConnection(selectedConnectionId ?? '', (connection) => ({
                        ...connection,
                        from: event.target.value,
                      }))
                    }
                  >
                    {Object.keys(patchDocument.patch.nodes).map((nodeId) => (
                      <option key={`from-${nodeId}`} value={nodeId}>
                        {nodeId}
                      </option>
                    ))}
                  </select>
                </label>
                <label className="patch-graph-control">
                  <span>Connection type</span>
                  <select
                    value={selectedConnection.kind}
                    onChange={(event) =>
                      onUpdateConnection(selectedConnectionId ?? '', (connection) => ({
                        ...connection,
                        kind: event.target.value as PatchConnectionKind,
                      }))
                    }
                  >
                    {CONNECTION_KIND_OPTIONS.map((option) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </select>
                </label>
                <label className="patch-graph-control patch-graph-control-full">
                  <span>Destination</span>
                  <select
                    value={selectedConnection.to}
                    onChange={(event) =>
                      onUpdateConnection(selectedConnectionId ?? '', (connection) => ({
                        ...connection,
                        to: event.target.value,
                      }))
                    }
                  >
                    {Object.keys(patchDocument.patch.nodes).map((nodeId) => (
                      <option key={`to-${nodeId}`} value={nodeId}>
                        {nodeId}
                      </option>
                    ))}
                  </select>
                </label>
              </div>
              <div className="patch-graph-connection-actions">
                <button className="danger" onClick={() => selectedConnectionId && onRemoveConnection(selectedConnectionId)}>
                  Remove connection
                </button>
              </div>
            </div>
          ) : null}

          {Object.entries(patchDocument.patch.nodes).map(([nodeId, node]) => {
            const position = positions[nodeId] ?? { x: 64, y: 64 };
            const selected = nodeId === selectedNodeId;
            const pending = nodeId === wireDraft?.fromNodeId;
            const wireTarget = nodeId === wireDraft?.targetNodeId;
            const nodeClassName = ['patch-graph-node'];
            if (selected) {
              nodeClassName.push('selected');
            }
            if (pending) {
              nodeClassName.push('pending');
            }
            if (wireTarget) {
              nodeClassName.push('wire-target');
            }

            return (
              <div
                key={nodeId}
                className={nodeClassName.join(' ')}
                style={{ left: position.x, top: position.y }}
                onClick={() => onSelectNode(nodeId)}
                title={`${nodeId}: ${node.type}. Click to inspect, drag the header to move it.`}
              >
                {canReceiveWire(node) ? (
                  <button
                    className={wireTarget ? 'patch-graph-socket input armed' : 'patch-graph-socket input'}
                    data-patch-input-node-id={nodeId}
                    onClick={(event) => {
                      event.stopPropagation();
                      onSelectNode(nodeId);
                    }}
                    title={`Use ${nodeId} as the destination`}
                  />
                ) : null}
                {canStartWire(node) ? (
                  <button
                    className={pending ? 'patch-graph-socket output armed' : 'patch-graph-socket output'}
                    onPointerDown={(event) => beginWireDrag(nodeId, event)}
                    onClick={(event) => {
                      event.stopPropagation();
                      onSelectNode(nodeId);
                    }}
                    title={`Drag from ${nodeId} to start a connection`}
                  />
                ) : null}
                <div className="patch-graph-node-header" onPointerDown={(event) => beginDrag(nodeId, event)}>
                  <div className="patch-graph-node-title">
                    <span className={`patch-graph-node-badge type-${node.type}`}>{NODE_BADGES[node.type]}</span>
                    <div className="patch-graph-node-labels">
                      <strong>{nodeId}</strong>
                      <span>{node.type}</span>
                    </div>
                  </div>
                </div>
                <div className="patch-graph-node-body">
                  {renderNodeControls(nodeId, node)}
                  <div className="patch-graph-node-footer">Inspector stays synced</div>
                </div>
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
}

export default PatchGraphCanvas;
