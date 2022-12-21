import type {FunctionCallDef, ServiceDef} from "@fluencelabs/fluence/dist/internal/compilerSupport/v3impl/interface"

export class AquaConfig {
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
    services: ServiceDef[]
    functions: { [key: string]: AquaFunction }
}

export class Compiler {
    compile(path: string, imports: string[], config?: AquaConfig): Promise<CompilationResult>;
    compileString(input: string, imports: string[], config?: AquaConfig): Promise<CompilationResult>;
}

export var Aqua: Compiler;
