import yaml from 'js-yaml';

export const PATCH_ROLE_OPTIONS = ['drums', 'bass', 'pad', 'arp', 'lead'] as const;

export type PatchRole = (typeof PATCH_ROLE_OPTIONS)[number];

export type PatchNodeType = 'oscillator' | 'noise' | 'envelope' | 'filter' | 'output';
export type PatchConnectionKind = 'signal' | 'amp_envelope' | 'filter_envelope' | 'hard_sync';
export type OscillatorWaveform = 'sine' | 'saw' | 'square' | 'triangle';
export type NoiseColor = 'white' | 'pink' | 'mix';
export type FilterMode = 'lowpass' | 'highpass' | 'bandpass';
export type FilterSlope = '12' | '24' | '36' | '48';
export type PlayMode = 'poly' | 'monolegato';
export type VoiceStealMode = 'quietest';

export type PatchPitch = {
  octaves: number;
  semitones: number;
  cents: number;
  hzOffset: number;
};

export type PatchOscillatorNode = {
  type: 'oscillator';
  waveform: OscillatorWaveform;
  level: number;
  pitch: PatchPitch;
};

export type PatchNoiseNode = {
  type: 'noise';
  color: NoiseColor;
  mix: number;
  level: number;
};

export type PatchEnvelopeNode = {
  type: 'envelope';
  attack: number;
  decay: number;
  sustain: number;
  release: number;
};

export type PatchFilterNode = {
  type: 'filter';
  mode: FilterMode;
  cutoffHz: number;
  q: number;
  slope: FilterSlope;
  keyTrack: number;
  envAmountOct: number;
  qEnvAmount: number;
};

export type PatchOutputNode = {
  type: 'output';
  gain: number;
  layerSpread: number;
  playMode: PlayMode;
  polyMax: number;
  voiceSteal: VoiceStealMode;
};

export type PatchNode =
  | PatchOscillatorNode
  | PatchNoiseNode
  | PatchEnvelopeNode
  | PatchFilterNode
  | PatchOutputNode;

export type PatchConnection = {
  from: string;
  to: string;
  kind: PatchConnectionKind;
};

export type PatchDocument = {
  patch: {
    name: string;
    nodes: Record<string, PatchNode>;
    connections: Record<string, PatchConnection>;
  };
};

type RawPatchFile = {
  patch?: {
    name?: unknown;
    nodes?: Record<string, Record<string, unknown>>;
    connections?: Record<string, Record<string, unknown>>;
  };
};

function makeId(prefix: string): string {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `${prefix}-${crypto.randomUUID().slice(0, 8)}`;
  }
  return `${prefix}-${Math.random().toString(36).slice(2, 10)}`;
}

function asText(value: unknown, fallback = ''): string {
  return typeof value === 'string' ? value.trim() : fallback;
}

function asFiniteNumber(value: unknown, fallback: number): number {
  if (typeof value === 'number' && Number.isFinite(value)) {
    return value;
  }
  if (typeof value === 'string' && value.trim() !== '') {
    const parsed = Number(value);
    if (Number.isFinite(parsed)) {
      return parsed;
    }
  }
  return fallback;
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

function ensureNodeType(type: string): PatchNodeType {
  switch (type) {
    case 'oscillator':
    case 'noise':
    case 'envelope':
    case 'filter':
    case 'output':
      return type;
    default:
      throw new Error(`Unsupported patch node type "${type}".`);
  }
}

function ensureConnectionKind(kind: string): PatchConnectionKind {
  switch (kind) {
    case 'signal':
    case 'amp_envelope':
    case 'filter_envelope':
    case 'hard_sync':
      return kind;
    default:
      throw new Error(`Unsupported connection kind "${kind}".`);
  }
}

function ensureOscillatorWaveform(waveform: string): OscillatorWaveform {
  switch (waveform) {
    case 'sine':
    case 'saw':
    case 'square':
    case 'triangle':
      return waveform;
    default:
      throw new Error(`Unsupported oscillator waveform "${waveform}".`);
  }
}

function ensureNoiseColor(color: string): NoiseColor {
  switch (color) {
    case 'white':
    case 'pink':
    case 'mix':
      return color;
    default:
      throw new Error(`Unsupported noise color "${color}".`);
  }
}

function ensureFilterMode(mode: string): FilterMode {
  switch (mode) {
    case 'lowpass':
    case 'highpass':
    case 'bandpass':
      return mode;
    default:
      throw new Error(`Unsupported filter mode "${mode}".`);
  }
}

function ensureFilterSlope(slope: string): FilterSlope {
  switch (slope) {
    case '12':
    case '24':
    case '36':
    case '48':
      return slope;
    default:
      throw new Error(`Unsupported filter slope "${slope}".`);
  }
}

function ensurePlayMode(mode: string): PlayMode {
  switch (mode) {
    case 'poly':
    case 'monolegato':
      return mode;
    default:
      throw new Error(`Unsupported play mode "${mode}".`);
  }
}

function ensureVoiceSteal(mode: string): VoiceStealMode {
  if (mode === 'quietest') {
    return mode;
  }
  throw new Error(`Unsupported voice steal mode "${mode}".`);
}

function defaultPitch(): PatchPitch {
  return {
    octaves: 0,
    semitones: 0,
    cents: 0,
    hzOffset: 0,
  };
}

export function createDefaultPatch(): PatchDocument {
  return {
    patch: {
      name: 'New Patch',
      nodes: {
        osc1: {
          type: 'oscillator',
          waveform: 'saw',
          level: 0.7,
          pitch: defaultPitch(),
        },
        amp_env: {
          type: 'envelope',
          attack: 0.02,
          decay: 0.4,
          sustain: 0.7,
          release: 0.8,
        },
        out: {
          type: 'output',
          gain: 0.9,
          layerSpread: 0,
          playMode: 'poly',
          polyMax: 8,
          voiceSteal: 'quietest',
        },
      },
      connections: {
        osc1_to_out: {
          from: 'osc1',
          to: 'out',
          kind: 'signal',
        },
        amp_to_out: {
          from: 'amp_env',
          to: 'out',
          kind: 'amp_envelope',
        },
      },
    },
  };
}

export function clonePatchDocument(document: PatchDocument): PatchDocument {
  return {
    patch: {
      name: document.patch.name,
      nodes: Object.fromEntries(
        Object.entries(document.patch.nodes).map(([id, node]) => [
          id,
          node.type === 'oscillator'
            ? { ...node, pitch: { ...node.pitch } }
            : {
                ...node,
              },
        ]),
      ),
      connections: Object.fromEntries(
        Object.entries(document.patch.connections).map(([id, connection]) => [id, { ...connection }]),
      ),
    },
  };
}

export function createPatchNode(nodeType: PatchNodeType): PatchNode {
  switch (nodeType) {
    case 'oscillator':
      return {
        type: 'oscillator',
        waveform: 'saw',
        level: 0.7,
        pitch: defaultPitch(),
      };
    case 'noise':
      return {
        type: 'noise',
        color: 'white',
        mix: 0.5,
        level: 0.15,
      };
    case 'envelope':
      return {
        type: 'envelope',
        attack: 0.02,
        decay: 0.4,
        sustain: 0.7,
        release: 0.8,
      };
    case 'filter':
      return {
        type: 'filter',
        mode: 'lowpass',
        cutoffHz: 1800,
        q: 0.707,
        slope: '24',
        keyTrack: 0,
        envAmountOct: 0,
        qEnvAmount: 0,
      };
    case 'output':
      return {
        type: 'output',
        gain: 0.9,
        layerSpread: 0,
        playMode: 'poly',
        polyMax: 8,
        voiceSteal: 'quietest',
      };
  }
}

export function createPatchConnection(from = '', to = '', kind: PatchConnectionKind = 'signal'): PatchConnection {
  return { from, to, kind };
}

function parseNode(rawNode: Record<string, unknown>): PatchNode {
  const nodeType = ensureNodeType(asText(rawNode.type));
  switch (nodeType) {
    case 'oscillator': {
      const pitch = (rawNode.pitch ?? {}) as Record<string, unknown>;
      return {
        type: 'oscillator',
        waveform: ensureOscillatorWaveform(asText(rawNode.waveform, 'saw').toLowerCase()),
        level: Math.max(0, asFiniteNumber(rawNode.level, 1)),
        pitch: {
          octaves: Math.round(asFiniteNumber(pitch.octaves, 0)),
          semitones: asFiniteNumber(pitch.semitones, 0),
          cents: asFiniteNumber(pitch.cents, 0),
          hzOffset: asFiniteNumber(pitch.hz_offset, 0),
        },
      };
    }
    case 'noise':
      return {
        type: 'noise',
        color: ensureNoiseColor(asText(rawNode.color, 'white').toLowerCase()),
        mix: clamp(asFiniteNumber(rawNode.mix, 0.5), 0, 1),
        level: Math.max(0, asFiniteNumber(rawNode.level, 1)),
      };
    case 'envelope':
      return {
        type: 'envelope',
        attack: Math.max(0, asFiniteNumber(rawNode.attack, 0.01)),
        decay: Math.max(0, asFiniteNumber(rawNode.decay, 0.2)),
        sustain: clamp(asFiniteNumber(rawNode.sustain, 0.7), 0, 1),
        release: Math.max(0, asFiniteNumber(rawNode.release, 0.4)),
      };
    case 'filter':
      return {
        type: 'filter',
        mode: ensureFilterMode(asText(rawNode.mode, 'lowpass').toLowerCase()),
        cutoffHz: Math.max(1, asFiniteNumber(rawNode.cutoff_hz, 1800)),
        q: Math.max(0.001, asFiniteNumber(rawNode.q, 0.707)),
        slope: ensureFilterSlope(asText(rawNode.slope, '24').toLowerCase()),
        keyTrack: clamp(asFiniteNumber(rawNode.key_track, 0), 0, 1),
        envAmountOct: asFiniteNumber(rawNode.env_amount_oct, 0),
        qEnvAmount: asFiniteNumber(rawNode.q_env_amount, 0),
      };
    case 'output':
      return {
        type: 'output',
        gain: Math.max(0, asFiniteNumber(rawNode.gain, 1)),
        layerSpread: clamp(asFiniteNumber(rawNode.layer_spread, 0), 0, 1),
        playMode: ensurePlayMode(asText(rawNode.play_mode, 'poly').toLowerCase()),
        polyMax: Math.max(1, Math.round(asFiniteNumber(rawNode.poly_max, 8))),
        voiceSteal: ensureVoiceSteal(asText(rawNode.voice_steal, 'quietest').toLowerCase()),
      };
  }
}

export function parsePatchText(contents: string): PatchDocument {
  const parsed = (yaml.load(contents) ?? {}) as RawPatchFile;
  if (!parsed.patch || typeof parsed.patch !== 'object') {
    throw new Error('YAML is missing a top-level patch block.');
  }

  const nodes = parsed.patch.nodes ?? {};
  const connections = parsed.patch.connections ?? {};
  if (Object.keys(nodes).length === 0) {
    throw new Error('Patch must define at least one node.');
  }

  return {
    patch: {
      name: asText(parsed.patch.name, 'Untitled Patch'),
      nodes: Object.fromEntries(Object.entries(nodes).map(([id, raw]) => [id, parseNode(raw)])),
      connections: Object.fromEntries(
        Object.entries(connections).map(([id, raw]) => [
          id,
          {
            from: asText(raw.from),
            to: asText(raw.to),
            kind: ensureConnectionKind(asText(raw.kind, 'signal').toLowerCase()),
          },
        ]),
      ),
    },
  };
}

function trimFloat(value: number): number {
  return Math.round(value * 1000) / 1000;
}

export function serializePatch(document: PatchDocument): string {
  const root = {
    patch: {
      name: document.patch.name.trim() || 'Untitled Patch',
      nodes: Object.fromEntries(
        Object.entries(document.patch.nodes).map(([id, node]) => {
          switch (node.type) {
            case 'oscillator':
              return [
                id,
                {
                  type: node.type,
                  waveform: node.waveform,
                  level: trimFloat(node.level),
                  pitch: {
                    octaves: node.pitch.octaves,
                    semitones: trimFloat(node.pitch.semitones),
                    cents: trimFloat(node.pitch.cents),
                    hz_offset: trimFloat(node.pitch.hzOffset),
                  },
                },
              ];
            case 'noise':
              return [
                id,
                {
                  type: node.type,
                  color: node.color,
                  mix: trimFloat(node.mix),
                  level: trimFloat(node.level),
                },
              ];
            case 'envelope':
              return [
                id,
                {
                  type: node.type,
                  attack: trimFloat(node.attack),
                  decay: trimFloat(node.decay),
                  sustain: trimFloat(node.sustain),
                  release: trimFloat(node.release),
                },
              ];
            case 'filter':
              return [
                id,
                {
                  type: node.type,
                  mode: node.mode,
                  cutoff_hz: trimFloat(node.cutoffHz),
                  q: trimFloat(node.q),
                  slope: node.slope,
                  key_track: trimFloat(node.keyTrack),
                  env_amount_oct: trimFloat(node.envAmountOct),
                  q_env_amount: trimFloat(node.qEnvAmount),
                },
              ];
            case 'output':
              return [
                id,
                {
                  type: node.type,
                  gain: trimFloat(node.gain),
                  layer_spread: trimFloat(node.layerSpread),
                  play_mode: node.playMode,
                  poly_max: node.polyMax,
                  voice_steal: node.voiceSteal,
                },
              ];
          }
        }),
      ),
      connections: Object.fromEntries(
        Object.entries(document.patch.connections).map(([id, connection]) => [
          id,
          {
            from: connection.from,
            to: connection.to,
            kind: connection.kind,
          },
        ]),
      ),
    },
  };

  return yaml.dump(root, {
    noRefs: true,
    lineWidth: -1,
    sortKeys: false,
  });
}

export function validatePatch(document: PatchDocument): string[] {
  const issues: string[] = [];
  const nodeEntries = Object.entries(document.patch.nodes);
  const connectionEntries = Object.entries(document.patch.connections);

  if (!document.patch.name.trim()) {
    issues.push('Patch name is required.');
  }
  if (nodeEntries.length === 0) {
    issues.push('Add at least one patch node.');
  }

  const outputNodes = nodeEntries.filter(([, node]) => node.type === 'output');
  if (outputNodes.length !== 1) {
    issues.push('Patch should define exactly one output node.');
  }

  const signalNodeCount = nodeEntries.filter(([, node]) => node.type === 'oscillator' || node.type === 'noise').length;
  if (signalNodeCount === 0) {
    issues.push('Patch should include at least one oscillator or noise node.');
  }
  if (signalNodeCount > 4) {
    issues.push('Patch currently supports at most four oscillator/noise layers.');
  }

  const nodeIds = new Set(nodeEntries.map(([id]) => id));
  const connectionIds = new Set<string>();
  for (const [connectionId, connection] of connectionEntries) {
    if (connectionIds.has(connectionId)) {
      issues.push(`Connection "${connectionId}" is duplicated.`);
    }
    connectionIds.add(connectionId);
    if (!connection.from.trim()) {
      issues.push(`Connection "${connectionId}" is missing a source node.`);
    }
    if (!connection.to.trim()) {
      issues.push(`Connection "${connectionId}" is missing a destination node.`);
    }
    if (connection.from && !nodeIds.has(connection.from)) {
      issues.push(`Connection "${connectionId}" references unknown source node "${connection.from}".`);
    }
    if (connection.to && !nodeIds.has(connection.to)) {
      issues.push(`Connection "${connectionId}" references unknown destination node "${connection.to}".`);
    }
  }

  for (const [nodeId, node] of nodeEntries) {
    switch (node.type) {
      case 'oscillator':
        if (node.level < 0) issues.push(`Oscillator "${nodeId}" level must be >= 0.`);
        break;
      case 'noise':
        if (node.mix < 0 || node.mix > 1) issues.push(`Noise "${nodeId}" mix must stay in [0,1].`);
        if (node.level < 0) issues.push(`Noise "${nodeId}" level must be >= 0.`);
        break;
      case 'envelope':
        if (node.sustain < 0 || node.sustain > 1) issues.push(`Envelope "${nodeId}" sustain must stay in [0,1].`);
        break;
      case 'filter':
        if (node.cutoffHz <= 0) issues.push(`Filter "${nodeId}" cutoff must be > 0.`);
        if (node.q <= 0) issues.push(`Filter "${nodeId}" Q must be > 0.`);
        if (node.keyTrack < 0 || node.keyTrack > 1) issues.push(`Filter "${nodeId}" key tracking must stay in [0,1].`);
        break;
      case 'output':
        if (node.layerSpread < 0 || node.layerSpread > 1) issues.push(`Output "${nodeId}" layer spread must stay in [0,1].`);
        if (node.polyMax <= 0) issues.push(`Output "${nodeId}" poly max must be > 0.`);
        break;
    }
  }

  return issues;
}

export function nextPatchNodeId(document: PatchDocument, prefix: string): string {
  let suffix = 1;
  let candidate = `${prefix}${suffix}`;
  while (document.patch.nodes[candidate]) {
    suffix += 1;
    candidate = `${prefix}${suffix}`;
  }
  return candidate;
}

export function nextPatchConnectionId(document: PatchDocument, prefix = 'connection'): string {
  let suffix = 1;
  let candidate = `${prefix}${suffix}`;
  while (document.patch.connections[candidate]) {
    suffix += 1;
    candidate = `${prefix}${suffix}`;
  }
  return candidate;
}

export function renamePatchNode(document: PatchDocument, currentId: string, nextId: string): PatchDocument {
  const trimmed = nextId.trim();
  if (!trimmed || trimmed === currentId || document.patch.nodes[trimmed]) {
    return document;
  }

  const next = clonePatchDocument(document);
  const node = next.patch.nodes[currentId];
  if (!node) {
    return next;
  }
  delete next.patch.nodes[currentId];
  next.patch.nodes[trimmed] = node;
  for (const connection of Object.values(next.patch.connections)) {
    if (connection.from === currentId) {
      connection.from = trimmed;
    }
    if (connection.to === currentId) {
      connection.to = trimmed;
    }
  }
  return next;
}

export function createSuggestedPatchFileName(name: string): string {
  const normalized = name
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
  return `${normalized || makeId('patch')}.yaml`;
}
