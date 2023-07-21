import { CompilationResult } from "./aqua-api";

type CommonArgs = {
  imports?: string[] | undefined;
  constants?: string[] | undefined;
  logLevel?: string | undefined;
  noRelay?: boolean | undefined;
  noXor?: boolean | undefined;
  targetType?: "ts" | "js" | "air";
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
    code: string;
  } & CommonArgs,
): Promise<CompilationResult>;
declare function compile(
  arg: {
    filePath: string;
  } & CommonArgs,
): Promise<CompilationResult>;
declare function compile(
  arg: {
    filePath: string;
    funcCall: string;
    data?: Record<string, unknown> | undefined;
  } & CommonArgs,
): Promise<Required<CompilationResult>>;
export = compile;
