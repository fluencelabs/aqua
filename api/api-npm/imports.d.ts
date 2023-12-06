import Arborist from "@npmcli/arborist";

export declare type Imports = Record<string, string[]>;

/**
 * Gather imports for aqua compiler from
 * actual node_modules folder created by npm.
 * Underneath it uses @npmcli/arborist.
 * @param path path to project with node_modules folder
 */
export declare async function gatherImportsFromNpm(path: string): Imports;

/**
 * Same as `gatherImportsFromNpm` but uses
 * already created arborist instance.
 * @param arborist arborist instance
 */
export declare async function gatherImportsFromArborist(
  arborist: Arborist,
): Imports;
