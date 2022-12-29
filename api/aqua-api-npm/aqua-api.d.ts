import type {FunctionCallDef, ServiceDef} from "@fluencelabs/fluence/dist/internal/compilerSupport/v3impl/interface"

export class AquaConfig {
    constructor(logLevel: string, constants: string[], noXor: boolean, noRelay: boolean);
    logLevel?: string
    constants?: string[]
    noXor?: boolean
    noRelay?: boolean
}

export class AquaFunction {
    funcDef: FunctionCallDef
    script: string
}

export class CompilationResult {
    services: Record<string, ServiceDef>
    functions: Record<string, AquaFunction>
    errors: string[]
}

export class CompilationRunResult {
    aquaFunction?: AquaFunction
    errors: string[]
}

export class Compiler {
    compileRun(functionStr: string, arguments: any, path: string, imports: string[], config?: AquaConfig): Promise<CompilationRunResult>;
    compile(path: string, imports: string[], config?: AquaConfig): Promise<CompilationResult>;
    compileString(input: string, imports: string[], config?: AquaConfig): Promise<CompilationResult>;
}

export var Aqua: Compiler;
