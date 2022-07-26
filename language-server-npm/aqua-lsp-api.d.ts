export interface TokenLocation {
    name: string,
    startLine: number,
    startCol: number,
    endLine: number,
    endCol: number
}

export interface TokenLink {
    current: TokenLocation,
    definition: TokenLocation
}

export interface TokenImport {
    current: TokenLocation,
    path: string
}

export interface ErrorInfo {
    start: number,
    end: number,
    message: string,
    location: string | null
}

export interface CompilationResult {
    errors: ErrorInfo[],
    locations: TokenLink[],
    importLocations: TokenImport[]
}

export class Compiler {
    compile(path: string, imports: string[]): Promise<CompilationResult>;
}

export var AquaLSP: Compiler;
