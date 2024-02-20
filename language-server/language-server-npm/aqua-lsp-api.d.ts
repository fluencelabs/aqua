export interface ScalarType {
    name: string,
    tag: "scalar"
}

export interface ArrayType {
    element: Type,
    tag: "array"
}

export interface OptionType {
    element: Type,
    tag: "option"
}

export interface StreamType {
    element: Type,
    tag: "stream"
}

export interface StreamMapType {
    element: Type,
    tag: "streammap"
}

export interface CanonStreamType {
    element: Type,
    tag: "canon"
}

export interface AbilityType {
    name: string,
    fields: Record<string, Type>,
    tag: "ability"
}

export interface StructType {
    name: string,
    fields: Record<string, Type>,
    tag: "struct"
}

export interface ServiceType {
    name: string,
    fields: Record<string, Type>,
    tag: "service"
}

export interface LabeledConsType {
    args: Record<string, Type>,
    tag: "labeled"
}

export interface UnlabeledConsType {
    types: Type[],
    tag: "unlabeled"
}

export interface ArrowType {
    domain: LabeledConsType
    codomain: UnlabeledConsType,
    tag: "arrow"
}

export interface NilType {
    tag: "nil"
}

export interface BottomType {
    tag: "bottom"
}

export interface TopType {
    tag: "top"
}

export type Type =
    ScalarType
    | ArrayType
    | OptionType
    | StreamType
    | StreamMapType
    | CanonStreamType
    | AbilityType
    | StructType
    | ServiceType
    | LabeledConsType
    | UnlabeledConsType
    | ArrowType
    | NilType
    | TopType
    | BottomType

export interface TokenLocation {
    name: string;
    startLine: number;
    startCol: number;
    endLine: number;
    endCol: number;
}

export interface TokenInfo {
    location: TokenLocation;
    type: Type;
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
