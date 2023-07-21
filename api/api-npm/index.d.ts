import { CompilationResult } from "./aqua-api";

type CommonArgs = {
  /** Paths to directories, which you want to import .aqua files from */
  imports?: string[] | undefined;
  /** Constants to be passed to the compiler */
  constants?: string[] | undefined;
  /** Set log level for the compiler. Must be one of: Must be one of: all, trace, debug, info, warn, error, off */
  logLevel?: string | undefined;
  /** Do not generate a pass through the relay node */
  noRelay?: boolean | undefined;
  /** Do not generate a wrapper that catches and displays errors */
  noXor?: boolean | undefined;
  /** If passed */
  targetType?: "ts" | "js" | "air";
  /** Compile aqua in tracing mode (for debugging purposes) */
  tracing?: boolean | undefined;
};

/**
 * Compile aqua code
 *
 * There are 3 ways to call the function:
 *
 * 1. Compile aqua code from string (use `code` field)
 * 1. Compile aqua code from file (use `filePath` field)
 * 1. Compile aqua function call from file (use `filePath`, `funcCall` and, optionally `data` fields)
 */
declare function compile(
  arg: {
    /** Aqua code string you want to compile */
    code: string;
  } & CommonArgs,
): Promise<CompilationResult>;
declare function compile(
  arg: {
    /** Path to the file you want to compile */
    filePath: string;
  } & CommonArgs,
): Promise<CompilationResult>;
declare function compile(
  arg: {
    /** Path to the file you want to compile */
    filePath: string;
    /** Function call you want to compile. Example: someFunc("someArg") */
    funcCall: string;
    /** Args to be passed to the function (record with keys named as args you want to pass) */
    data?: Record<string, unknown> | undefined;
  } & CommonArgs,
): Promise<Required<CompilationResult>>;
export = compile;
