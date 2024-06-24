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
