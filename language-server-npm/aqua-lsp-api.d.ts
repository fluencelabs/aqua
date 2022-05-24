export interface TokenLocation {
    name: string,
    start: number,
    end: number
}

export interface TokenLink {
    current: TokenLocation,
    definition: TokenLocation
}

export interface ErrorInfo {
    start: number,
    end: number,
    message: string,
    location: string | null
}

export interface CompilationResult {
    errors: ErrorInfo[],
    locations: TokenLink[]
}

export class Compiler {
    compile(path: string, imports: string[]): Promise<CompilationResult>;
}

export var AquaLSP: Compiler;
