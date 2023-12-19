export interface TokenLocation {
  name: string;
  startLine: number;
  startCol: number;
  endLine: number;
  endCol: number;
}

export interface TokenInfo {
  location: TokenLocation;
  type: string;
}

export interface TokenLink {
  current: TokenLocation;
  definition: TokenLocation;
}

export interface TokenImport {
  current: TokenLocation;
  path: string;
}

export interface ErrorInfo {
  infoType: "error";
  start: number;
  end: number;
  message: string;
  location: string | null;
}

export interface WarningInfo {
  infoType: "warning";
  start: number;
  end: number;
  message: string;
  location: string | null;
}

export interface CompilationResult {
  errors: ErrorInfo[];
  warnings: WarningInfo[];
  locations: TokenLink[];
  importLocations: TokenImport[];
  tokens: TokenInfo[];
}

/*
 * Imports configuration for the compiler.
 * Structure:
 * {
 *  "<compiled-path-prefix-1>": {
 *   "<import-path-prefix-1>": ["<import-path-1>", "<import-path-2>"],
 *   "<import-path-prefix-2>": "<import-path-3>",
 *   ...
 *  }
 *  ...
 * }
 */
export type Imports = Record<string, Record<string, string[]>>;

export class Compiler {
  compile(path: string, imports: Imports): Promise<CompilationResult>;
}

export var AquaLSP: Compiler;
