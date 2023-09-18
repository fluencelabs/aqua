import { ServiceDef, FunctionCallDef } from "@fluencelabs/interfaces";

export class AquaFunction {
  funcDef: FunctionCallDef;
  script: string;
}

export class GeneratedSource {
  name: string;
  tsSource?: string;
  jsSource?: string;
  tsTypes?: string;
}

export class CompilationResult {
  services: Record<string, ServiceDef>;
  functions: Record<string, AquaFunction>;
  functionCall?: AquaFunction;
  errors: string[];
  generatedSources: GeneratedSource[];
}

/** Common arguments for all compile functions */
type CommonArgs = {
  /** Paths to directories, which you want to import .aqua files from. Example: ["./path/to/dir"] */
  imports?: string[] | undefined;
  /** Constants to be passed to the compiler. Example: ["CONSTANT1=1", "CONSTANT2=2"] */
  constants?: string[] | undefined;
  /** Set log level for the compiler. Must be one of: Must be one of: all, trace, debug, info, warn, error, off. Default: info */
  logLevel?: string | undefined;
  /** Do not generate a pass through the relay node. Default: false */
  noRelay?: boolean | undefined;
  /** Do not generate a wrapper that catches and displays errors. Default: false */
  noXor?: boolean | undefined;
  /** Target type for the compiler. Must be one of: ts, js, air. Default: air */
  targetType?: "ts" | "js" | "air" | undefined;
  /** Compile aqua in tracing mode (for debugging purposes). Default: false */
  tracing?: boolean | undefined;
};

type CodeString = {
  /** Aqua code to be compiled */
  code: string;
};

export type CompileFromStringArgs = CommonArgs & CodeString;

/** Compile aqua code from a string */
export declare function compileFromString(
  args: CompileFromStringArgs,
): Promise<Omit<CompilationResult, "funcCall">>;

type FilePath = {
  /** Path to the aqua file to be compiled */
  filePath: string;
};

export type CompileFromPathArgs = CommonArgs & FilePath;

/** Compile aqua code from a file */
export declare function compileFromPath(
  args: CompileFromPathArgs,
): Promise<Omit<CompilationResult, "funcCall">>;

type FuncCall = {
  /** Function call you want to compile. Example: someFunc("someArg") */
  funcCall: string;
  /** Args to be passed to the function (record with keys named as args you want to pass to the function) Example: { someArg: 1 } */
  data?: Record<string, unknown> | undefined;
};

export type CompileFuncCallFromStringArgs = CommonArgs & CodeString & FuncCall;

/** Compile aqua function call from a string */
export declare function compileAquaCallFromString(
  args: CompileFuncCallFromStringArgs,
): Promise<Required<CompilationResult>>;

export type CompileFuncCallFromPathArgs = CommonArgs & FilePath & FuncCall;

/** Compile aqua function call from a file */
export declare function compileAquaCallFromPath(
  args: CompileFuncCallFromPathArgs,
): Promise<Required<CompilationResult>>;

export {};
