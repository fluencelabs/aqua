/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import { ServiceDef, FunctionCallDef } from "@fluencelabs/interfaces";

export declare class AquaFunction {
  funcDef: FunctionCallDef;
  script: string;
}

export declare class GeneratedSource {
  name: string;
  tsSource?: string;
  jsSource?: string;
  tsTypes?: string;
}

export declare class CompilationResult {
  services: Record<string, ServiceDef>;
  functions: Record<string, AquaFunction>;
  errors: string[];
  warnings: string[];
  generatedSources: GeneratedSource[];
}

/**
 * Imports configuration for the compiler.
 * Structure:
 * {
 *  "<compiled-path-prefix-1>": {
 *   "<import-path-prefix-1>": ["<import-path-1>", "<import-path-2>"],
 *   "<import-path-prefix-2>": "<import-path-3>",
 *   ...
 *  }
 *  ...
 * }
 * Import `import` written in file with path `path`
 * is resolved as follows:
 * 1. Try to resolve `import` as relative import from `path`
 * 2. If relative resolution failed:
 *  a. Find **the longest** <compiled-path-prefix>
 *     that is a prefix of `path` in the imports configuration
 *  b. In obtained map, find **the longest** <import-path-prefix>
 *     that is a prefix of `import`
 *  c. Replace prefix in `import` with <import-path>
 *  d. Try to resolve import with obtained path
 *     (try a few paths if array was provided)
 *
 * WARNING: <compiled-path-prefix> in 2.a is compared with
 *          absolute normalized path of `path`, so <compiled-path-prefix>
 *          should be absolute normalized path as well
 * NOTE: <import-path-prefix> could be empty string,
 *       in which case it will match any import
 * NOTE: passing just an array of strings is a shorthand for
 * {
 *   "/": {
 *     "": <array>
 *   }
 * }
 */
type Imports = Record<string, Record<string, string[] | string>> | string[];

/** Common arguments for all compile functions */
type CommonArgs = {
  /** Imports */
  imports?: Imports | undefined;
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
  /** Do not generate response call if there are no returned values */
  noEmptyResponse?: boolean | undefined;
};

type CodeString = {
  /** Aqua code to be compiled */
  code: string;
};

export type CompileFromStringArgs = CommonArgs & CodeString;

/** Compile aqua code from a string */
export declare function compileFromString(
  args: CompileFromStringArgs,
): Promise<CompilationResult>;

type FilePath = {
  /** Path to the aqua file to be compiled */
  filePath: string;
};

export type CompileFromPathArgs = CommonArgs & FilePath;

/** Compile aqua code from a file */
export declare function compileFromPath(
  args: CompileFromPathArgs,
): Promise<CompilationResult>;

type FuncCall = {
  /** Function call you want to compile. Example: someFunc("someArg") */
  funcCall: string;
  /** Args to be passed to the function (record with keys named as args you want to pass to the function) Example: { someArg: 1 } */
  data?: Record<string, unknown> | undefined;
};

export type CallCompilationResult = CompilationResult & {
  functionCall: AquaFunction;
};

export type CompileFuncCallFromStringArgs = CommonArgs & CodeString & FuncCall;

/** Compile aqua function call from a string */
export declare function compileAquaCallFromString(
  args: CompileFuncCallFromStringArgs,
): Promise<CallCompilationResult>;

export type CompileFuncCallFromPathArgs = CommonArgs & FilePath & FuncCall;

/** Compile aqua function call from a file */
export declare function compileAquaCallFromPath(
  args: CompileFuncCallFromPathArgs,
): Promise<CallCompilationResult>;

export {};
