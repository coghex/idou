import yaml from 'js-yaml';
import type { ValidationIssue } from './validation';

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
export type PatchModulationSource =
  | 'lfo1'
  | 'amp_envelope'
  | 'filter_envelope'
  | 'key_track'
  | 'channel_aftertouch'
  | 'poly_aftertouch';
export type PatchModulationDestination = 'pitch_cents' | 'filter_cutoff_oct' | 'amp_gain';

export type PatchPitch = {
  octaves: number;
  semitones: number;
  cents: number;
  hzOffset: number;
};

export type PatchEnvelopeShape = {
  attack: number;
  decay: number;
  sustain: number;
  release: number;
};

export type PatchOscillatorNode = {
  type: 'oscillator';
  waveform: OscillatorWaveform;
  level: number;
  pitch: PatchPitch;
  ampEnvelope: PatchEnvelopeShape | null;
};

export type PatchNoiseNode = {
  type: 'noise';
  color: NoiseColor;
  mix: number;
  level: number;
  ampEnvelope: PatchEnvelopeShape | null;
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
  layerTarget: string;
  envAmountOct: number;
  qEnvAmount: number;
};

export type PatchOutputNode = {
  type: 'output';
  gain: number;
  layerSpread: number;
  lfo1RateHz: number;
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

export type PatchModulation = {
  source: PatchModulationSource;
  target: string;
  destination: PatchModulationDestination;
  amount: number;
};

export type PatchDocument = {
  patch: {
    name: string;
    nodes: Record<string, PatchNode>;
    connections: Record<string, PatchConnection>;
    modulations: Record<string, PatchModulation>;
  };
};

type RawPatchFile = {
  patch?: {
    name?: unknown;
    nodes?: Record<string, Record<string, unknown>>;
    connections?: Record<string, Record<string, unknown>>;
    modulations?: Record<string, Record<string, unknown>>;
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

function ensureModulationSource(source: string): PatchModulationSource {
  switch (source) {
    case 'lfo1':
    case 'amp_envelope':
    case 'filter_envelope':
    case 'key_track':
    case 'channel_aftertouch':
    case 'poly_aftertouch':
      return source;
    default:
      throw new Error(`Unsupported modulation source "${source}".`);
  }
}

function ensureModulationDestination(destination: string): PatchModulationDestination {
  switch (destination) {
    case 'pitch_cents':
    case 'filter_cutoff_oct':
    case 'amp_gain':
      return destination;
    default:
      throw new Error(`Unsupported modulation destination "${destination}".`);
  }
}

function defaultPitch(): PatchPitch {
  return {
    octaves: 0,
    semitones: 0,
    cents: 0,
    hzOffset: 0,
  };
}

function defaultEnvelopeShape(): PatchEnvelopeShape {
  return {
    attack: 0.02,
    decay: 0.4,
    sustain: 0.7,
    release: 0.8,
  };
}

function parseEnvelopeShape(rawNode: Record<string, unknown>, key: string): PatchEnvelopeShape | null {
  const rawEnvelope = rawNode[key];
  if (!rawEnvelope || typeof rawEnvelope !== 'object') {
    return null;
  }
  const envelope = rawEnvelope as Record<string, unknown>;
  return {
    attack: Math.max(0, asFiniteNumber(envelope.attack, 0.01)),
    decay: Math.max(0, asFiniteNumber(envelope.decay, 0.2)),
    sustain: clamp(asFiniteNumber(envelope.sustain, 0.7), 0, 1),
    release: Math.max(0, asFiniteNumber(envelope.release, 0.4)),
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
          ampEnvelope: null,
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
          lfo1RateHz: 0,
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
      modulations: {},
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
            ? { ...node, pitch: { ...node.pitch }, ampEnvelope: node.ampEnvelope ? { ...node.ampEnvelope } : null }
            : node.type === 'noise'
              ? { ...node, ampEnvelope: node.ampEnvelope ? { ...node.ampEnvelope } : null }
            : {
                ...node,
              },
        ]),
      ),
      connections: Object.fromEntries(
        Object.entries(document.patch.connections).map(([id, connection]) => [id, { ...connection }]),
      ),
      modulations: Object.fromEntries(
        Object.entries(document.patch.modulations).map(([id, modulation]) => [id, { ...modulation }]),
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
        ampEnvelope: null,
      };
    case 'noise':
      return {
        type: 'noise',
        color: 'white',
        mix: 0.5,
        level: 0.15,
        ampEnvelope: null,
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
        layerTarget: 'all',
        envAmountOct: 0,
        qEnvAmount: 0,
      };
    case 'output':
      return {
        type: 'output',
        gain: 0.9,
        layerSpread: 0,
        lfo1RateHz: 0,
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
        ampEnvelope: parseEnvelopeShape(rawNode, 'amp_envelope'),
      };
    }
    case 'noise':
      return {
        type: 'noise',
        color: ensureNoiseColor(asText(rawNode.color, 'white').toLowerCase()),
        mix: clamp(asFiniteNumber(rawNode.mix, 0.5), 0, 1),
        level: Math.max(0, asFiniteNumber(rawNode.level, 1)),
        ampEnvelope: parseEnvelopeShape(rawNode, 'amp_envelope'),
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
        layerTarget: asText(rawNode.layer_target, 'all') || 'all',
        envAmountOct: asFiniteNumber(rawNode.env_amount_oct, 0),
        qEnvAmount: asFiniteNumber(rawNode.q_env_amount, 0),
      };
    case 'output':
      return {
        type: 'output',
        gain: Math.max(0, asFiniteNumber(rawNode.gain, 1)),
        layerSpread: clamp(asFiniteNumber(rawNode.layer_spread, 0), 0, 1),
        lfo1RateHz: Math.max(0, asFiniteNumber(rawNode.lfo1_rate_hz, 0)),
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
  const modulations = parsed.patch.modulations ?? {};
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
      modulations: Object.fromEntries(
        Object.entries(modulations).map(([id, raw]) => [
          id,
          {
            source: ensureModulationSource(asText(raw.source).toLowerCase()),
            target: asText(raw.target),
            destination: ensureModulationDestination(asText(raw.destination).toLowerCase()),
            amount: asFiniteNumber(raw.amount, 0),
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
                  ...(node.ampEnvelope
                    ? {
                        amp_envelope: {
                          attack: trimFloat(node.ampEnvelope.attack),
                          decay: trimFloat(node.ampEnvelope.decay),
                          sustain: trimFloat(node.ampEnvelope.sustain),
                          release: trimFloat(node.ampEnvelope.release),
                        },
                      }
                    : {}),
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
                  ...(node.ampEnvelope
                    ? {
                        amp_envelope: {
                          attack: trimFloat(node.ampEnvelope.attack),
                          decay: trimFloat(node.ampEnvelope.decay),
                          sustain: trimFloat(node.ampEnvelope.sustain),
                          release: trimFloat(node.ampEnvelope.release),
                        },
                      }
                    : {}),
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
                  ...(node.layerTarget !== 'all' ? { layer_target: node.layerTarget } : {}),
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
                  lfo1_rate_hz: trimFloat(node.lfo1RateHz),
                  play_mode: node.playMode,
                  poly_max: node.polyMax,
                  voice_steal: node.voiceSteal,
                },
              ];
            default: {
              const _exhaustive: never = node;
              throw new Error(`Unknown patch node type: ${(_exhaustive as PatchNode).type}`);
            }
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
      modulations: Object.fromEntries(
        Object.entries(document.patch.modulations).map(([id, modulation]) => [
          id,
          {
            source: modulation.source,
            target: modulation.target,
            destination: modulation.destination,
            amount: trimFloat(modulation.amount),
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

function patchIssueId(...parts: Array<string | number>): string {
  return ['sound', ...parts].join(':');
}

function createPatchIssue(
  id: string,
  title: string,
  message: string,
  focus: ValidationIssue['focus'] = { kind: 'sound' },
): ValidationIssue {
  return {
    id,
    scope: 'sound',
    severity: 'error',
    title,
    message,
    focus,
  };
}

export function validatePatch(document: PatchDocument): ValidationIssue[] {
  const issues: ValidationIssue[] = [];
  const nodeEntries = Object.entries(document.patch.nodes);
  const connectionEntries = Object.entries(document.patch.connections);
  const modulationEntries = Object.entries(document.patch.modulations);

  if (!document.patch.name.trim()) {
    issues.push(createPatchIssue(patchIssueId('name', 'missing'), 'Current sound draft', 'Sound name is required.'));
  }
  if (nodeEntries.length === 0) {
    issues.push(createPatchIssue(patchIssueId('nodes', 'missing'), 'Current sound draft', 'Add at least one sound block.'));
  }

  const outputNodes = nodeEntries.filter(([, node]) => node.type === 'output');
  if (outputNodes.length !== 1) {
    issues.push(
      createPatchIssue(
        patchIssueId('output', 'count'),
        'Current sound draft',
        'Define exactly one output block.',
      ),
    );
  }

  const signalNodeCount = nodeEntries.filter(([, node]) => node.type === 'oscillator' || node.type === 'noise').length;
  if (signalNodeCount === 0) {
    issues.push(
      createPatchIssue(
        patchIssueId('signal-source', 'missing'),
        'Current sound draft',
        'Add at least one oscillator or noise block.',
      ),
    );
  }
  if (signalNodeCount > 4) {
    issues.push(
      createPatchIssue(
        patchIssueId('signal-source', 'too-many'),
        'Current sound draft',
        'Current sound drafts support at most four oscillator/noise layers.',
      ),
    );
  }

  const nodeIds = new Set(nodeEntries.map(([id]) => id));
  const connectionIds = new Set<string>();
  for (const [connectionId, connection] of connectionEntries) {
    const connectionFocus = { kind: 'sound-connection', connectionId } as const;
    const connectionTitle = `Connection "${connectionId}"`;
    if (connectionIds.has(connectionId)) {
      issues.push(createPatchIssue(patchIssueId('connection', connectionId, 'duplicate'), connectionTitle, 'Connection is duplicated.', connectionFocus));
    }
    connectionIds.add(connectionId);
    if (!connection.from.trim()) {
      issues.push(
        createPatchIssue(
          patchIssueId('connection', connectionId, 'missing-source'),
          connectionTitle,
          'Source block is missing.',
          connectionFocus,
        ),
      );
    }
    if (!connection.to.trim()) {
      issues.push(
        createPatchIssue(
          patchIssueId('connection', connectionId, 'missing-destination'),
          connectionTitle,
          'Destination block is missing.',
          connectionFocus,
        ),
      );
    }
    if (connection.from && !nodeIds.has(connection.from)) {
      issues.push(
        createPatchIssue(
          patchIssueId('connection', connectionId, 'unknown-source'),
          connectionTitle,
          `Unknown source block "${connection.from}".`,
          connectionFocus,
        ),
      );
    }
    if (connection.to && !nodeIds.has(connection.to)) {
      issues.push(
        createPatchIssue(
          patchIssueId('connection', connectionId, 'unknown-destination'),
          connectionTitle,
          `Unknown destination block "${connection.to}".`,
          connectionFocus,
        ),
      );
    }
  }

  for (const [nodeId, node] of nodeEntries) {
    const nodeFocus = { kind: 'sound-node', nodeId } as const;
    const hasInlineAmpEnvelope =
      (node.type === 'oscillator' || node.type === 'noise') && node.ampEnvelope !== null;
    const hasLayerAmpEnvelopeConnection = connectionEntries.some(
      ([, connection]) => connection.kind === 'amp_envelope' && connection.to === nodeId,
    );
    switch (node.type) {
      case 'oscillator':
        if (node.level < 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'level'), `Oscillator "${nodeId}"`, 'Level must be 0 or greater.', nodeFocus));
        }
        if (node.ampEnvelope && (node.ampEnvelope.sustain < 0 || node.ampEnvelope.sustain > 1)) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'inline-envelope-sustain'), `Oscillator "${nodeId}"`, 'Inline amp-envelope sustain must stay between 0 and 1.', nodeFocus));
        }
        if (hasInlineAmpEnvelope && hasLayerAmpEnvelopeConnection) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'inline-envelope-conflict'), `Oscillator "${nodeId}"`, 'Use either an inline amp envelope or an amp-envelope connection for this layer, not both.', nodeFocus));
        }
        break;
      case 'noise':
        if (node.mix < 0 || node.mix > 1) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'mix'), `Noise "${nodeId}"`, 'Mix must stay between 0 and 1.', nodeFocus));
        }
        if (node.level < 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'level'), `Noise "${nodeId}"`, 'Level must be 0 or greater.', nodeFocus));
        }
        if (node.ampEnvelope && (node.ampEnvelope.sustain < 0 || node.ampEnvelope.sustain > 1)) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'inline-envelope-sustain'), `Noise "${nodeId}"`, 'Inline amp-envelope sustain must stay between 0 and 1.', nodeFocus));
        }
        if (hasInlineAmpEnvelope && hasLayerAmpEnvelopeConnection) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'inline-envelope-conflict'), `Noise "${nodeId}"`, 'Use either an inline amp envelope or an amp-envelope connection for this layer, not both.', nodeFocus));
        }
        break;
      case 'envelope':
        if (node.sustain < 0 || node.sustain > 1) {
          issues.push(
            createPatchIssue(
              patchIssueId('node', nodeId, 'sustain'),
              `Envelope "${nodeId}"`,
              'Sustain must stay between 0 and 1.',
              nodeFocus,
            ),
          );
        }
        break;
      case 'filter':
        if (node.cutoffHz <= 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'cutoff'), `Filter "${nodeId}"`, 'Cutoff must be greater than 0 Hz.', nodeFocus));
        }
        if (node.q <= 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'q'), `Filter "${nodeId}"`, 'Q must be greater than 0.', nodeFocus));
        }
        if (node.keyTrack < 0 || node.keyTrack > 1) {
          issues.push(
            createPatchIssue(
              patchIssueId('node', nodeId, 'key-track'),
              `Filter "${nodeId}"`,
              'Key tracking must stay between 0 and 1.',
              nodeFocus,
            ),
          );
        }
        if (!node.layerTarget.trim()) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'layer-target-empty'), `Filter "${nodeId}"`, 'Filter layer target must not be empty.', nodeFocus));
        } else if (node.layerTarget !== 'all') {
          const targetNode = document.patch.nodes[node.layerTarget];
          if (!targetNode) {
            issues.push(
              createPatchIssue(
                patchIssueId('node', nodeId, 'layer-target-missing'),
                `Filter "${nodeId}"`,
                `Filter layer target "${node.layerTarget}" must reference an existing oscillator or noise block.`,
                nodeFocus,
              ),
            );
          } else if (targetNode.type !== 'oscillator' && targetNode.type !== 'noise') {
            issues.push(
              createPatchIssue(
                patchIssueId('node', nodeId, 'layer-target-kind'),
                `Filter "${nodeId}"`,
                'Filter layer target must point at an oscillator or noise block.',
                nodeFocus,
              ),
            );
          }
        }
        break;
      case 'output':
        if (node.layerSpread < 0 || node.layerSpread > 1) {
          issues.push(
            createPatchIssue(
              patchIssueId('node', nodeId, 'layer-spread'),
              `Output "${nodeId}"`,
              'Layer spread must stay between 0 and 1.',
              nodeFocus,
            ),
          );
        }
        if (node.lfo1RateHz < 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'lfo1-rate'), `Output "${nodeId}"`, 'LFO1 rate must be 0 or greater.', nodeFocus));
        }
        if (node.polyMax <= 0) {
          issues.push(createPatchIssue(patchIssueId('node', nodeId, 'poly-max'), `Output "${nodeId}"`, 'Poly max must be greater than 0.', nodeFocus));
        }
        break;
    }
  }

  if (modulationEntries.length > 8) {
    issues.push(
      createPatchIssue(
        patchIssueId('modulations', 'too-many'),
        'Current sound draft',
        'Current sound drafts support at most eight modulation routes.',
      ),
    );
  }

  for (const [modulationId, modulation] of modulationEntries) {
    const modulationFocus = { kind: 'sound' } as const;
    if (!modulation.target.trim()) {
      issues.push(
        createPatchIssue(
          patchIssueId('modulation', modulationId, 'missing-target'),
          `Modulation "${modulationId}"`,
          'Target block is required.',
          modulationFocus,
        ),
      );
    } else if (!nodeIds.has(modulation.target)) {
      issues.push(
        createPatchIssue(
          patchIssueId('modulation', modulationId, 'unknown-target'),
          `Modulation "${modulationId}"`,
          `Unknown target block "${modulation.target}".`,
          modulationFocus,
        ),
      );
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
  for (const modulation of Object.values(next.patch.modulations)) {
    if (modulation.target === currentId) {
      modulation.target = trimmed;
    }
  }
  for (const node of Object.values(next.patch.nodes)) {
    if (node.type === 'filter' && node.layerTarget === currentId) {
      node.layerTarget = trimmed;
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
