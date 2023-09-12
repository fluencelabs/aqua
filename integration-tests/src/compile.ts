import { compileFromPath } from "@fluencelabs/aqua-api";
import { mkdir, writeFile } from "node:fs/promises";
import { resolve, parse, format, dirname } from "path";

const inputPath = resolve("./aqua");

const result = await compileFromPath({
  filePath: "aqua",
  imports: ["node_modules"],
  targetType: "ts",
});

const outputPath = resolve("./src/compiled");
await mkdir(outputPath, { recursive: true });

for (const src of result.generatedSources) {
  const outFilePath = resolve(src.name).replace(inputPath, outputPath);
  const outputTsPath = format({ ...parse(outFilePath), base: "", ext: ".ts" });

  await mkdir(dirname(outputTsPath), { recursive: true });
  await writeFile(outputTsPath, src.tsSource);
}
