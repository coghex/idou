export type ValidationSeverity = 'error' | 'warning' | 'success' | 'info';

export type ValidationScope = 'song' | 'sound' | 'engine';

export type ValidationFocus =
  | { kind: 'song' }
  | { kind: 'song-section'; sectionName: string }
  | { kind: 'sound' }
  | { kind: 'sound-node'; nodeId: string }
  | { kind: 'sound-connection'; connectionId: string };

export type ValidationIssue = {
  id: string;
  scope: ValidationScope;
  severity: ValidationSeverity;
  title: string;
  message: string;
  focus?: ValidationFocus;
};
