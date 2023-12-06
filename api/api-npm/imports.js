// @ts-check

import Arborist from "@npmcli/arborist";
import { breadth } from "treeverse";

export async function gatherImportsFromNpm(path) {
  const arb = new Arborist({ path });

  return await gatherImportsFromArborist(arb);
}

export async function gatherImportsFromArborist(arborist) {
  const tree = await arborist.loadActual();

  /**
   * Traverse dependency tree to construct map
   * (real path of a package) -> (real paths of its immediate dependencies)
   */
  let result = new Map();
  breadth({
    tree,
    getChildren(node, _) {
      let deps = [];
      for (let edge of node.edgesOut.values()) {
        // Skip dependencies that are not installed.
        if (edge.to === null) continue;
        // NOTE: Any errors in edge are ignored.
        const dep = edge.to;

        // Gather dependencies to traverse them.
        deps.push(dep);
        // Gather dependencies real paths.
        result.set(node.realpath, [
          ...(result.get(node.realpath) || []),
          dep.realpath,
        ]);
      }

      return deps;
    },
  });

  // Convert map to object.
  return Object.fromEntries(result);
}
