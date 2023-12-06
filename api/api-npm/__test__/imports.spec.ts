import { gatherImportsFromNpm } from "../imports.js";

describe("imports", () => {
  /**
   * NOTE: This test expects that `npm i` is run
   * inside `./__test__/data/transitive-deps/project` folder
   */
  it("should resolve transitive dependencies", async () => {
    const imports = await gatherImportsFromNpm(
      "./__test__/data/transitive-deps/project",
    );

    const projectPath = Object.keys(imports).find((p) => p.endsWith("project"));

    expect(projectPath).toBeDefined();

    const projectImports = imports[projectPath!];

    expect(projectImports.length).toEqual(2);

    const [dep1Path, dep2Path] = projectImports;

    const dep1Imports = imports[dep1Path];
    const dep2Imports = imports[dep2Path];

    expect(dep1Imports.length).toEqual(1);
    expect(dep2Imports.length).toEqual(1);

    const [dep1Transitive] = dep1Imports;
    const [dep2Transitive] = dep2Imports;

    expect(dep1Transitive.endsWith("C")).toBeTruthy();
    expect(dep2Transitive.endsWith("C")).toBeTruthy();

    // Transitive dependency should be resolved to different paths
    expect(dep1Transitive).not.toEqual(dep2Transitive);
  });
});
