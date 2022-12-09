import type {FunctionCallDef, ServiceDef} from "@fluencelabs/fluence/dist/internal/compilerSupport/v3impl/interface"

export interface Config {
    logLevel: string,
    constants: string[],
    noXor: boolean,
    noRelay: boolean
}

export interface AquaFunction {
    funcDef: FunctionCallDef,
    script: string
}

export interface CompilationResult {
    services: ServiceDef[],
    functions: Map<string, AquaFunction>
}

export class Compiler {
    compile(path: string, imports: string[], config: Config): Promise<CompilationResult>;
}

export var Aqua: Compiler;
