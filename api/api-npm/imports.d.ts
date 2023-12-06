import Arborist from "@npmcli/arborist";

import type { Imports } from "./index.d.ts";

export declare async function gatherImportsFromNpm(path: string): Imports;

export declare async function gatherImportsFromArborist(
  arborist: Arborist,
): Imports;
