export interface ErrorInfo {
    start: number,
    end: number,
    message: string,
    location: string | null
}

export class Compiler {
    compile(path: string, imports: string[]): Promise<ErrorInfo[]>;
}

export var AquaLSP: Compiler;
