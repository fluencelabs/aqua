import { compileFromPath } from "@fluencelabs/aqua-api";
import { mkdir, writeFile } from "node:fs/promises";
import { resolve, parse, format, dirname } from "node:path";

const inputPath = resolve("./aqua");

const result = await compileFromPath({
  filePath: "aqua",
  imports: ["node_modules"],
  targetType: "ts",
});

const outputPath = resolve("./src/compiled");
await mkdir(outputPath, { recursive: true });

if (result.errors.length > 0) {
  throw new Error(result.errors.join("\n"));
}

await Promise.all(
  result.generatedSources.map(async (src) => {
    const outFilePath = resolve(src.name).replace(inputPath, outputPath);
    const outputTsPath = format({
      ...parse(outFilePath),
      base: "",
      ext: ".ts",
    });
    await mkdir(dirname(outputTsPath), { recursive: true });
    await writeFile(outputTsPath, src.tsSource);
  }),
);
