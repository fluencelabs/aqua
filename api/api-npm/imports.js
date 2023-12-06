// @ts-check

import Arborist from "@npmcli/arborist";
import { breadth } from "treeverse";

export async function gatherImportsFromNpm(path) {
  const arb = new Arborist({ path });

  return await gatherImportsFromArborist(arb);
}

export async function gatherImportsFromArborist(arborist) {
  const tree = await arborist.loadActual();

  let result = new Map();
  breadth({
    tree,
    getChildren(node, _) {
      let deps = [];
      for (let edge of node.edgesOut.values()) {
        if (edge.to === null) continue;
        const dep = edge.to;

        deps.push(dep);
        result.set(node.realpath, [
          ...(result.get(node.realpath) || []),
          dep.realpath,
        ]);
      }
      return deps;
    },
  });

  return Object.fromEntries(result);
}
