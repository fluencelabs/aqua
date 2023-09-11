import "@fluencelabs/aqua-api"
import {compileFromPath} from "@fluencelabs/aqua-api";
import { promises as fs } from 'fs';
import * as Path from "path";

async function main() {
    const inputPath = Path.resolve("./aqua")
    const result = await compileFromPath({
        filePath: "aqua",
        imports: ["node_modules"],
        targetType: "ts"
    })

    const outputPath = Path.resolve("./src/compiled")
    await fs.mkdir(outputPath, {recursive: true})

    for (const src of result.generatedSources) {

        const outFilePath = Path.resolve(src.name).replace(inputPath, outputPath)
        const outputTsPath = Path.format({ ...Path.parse(outFilePath), base: '', ext: '.ts' })
        console.log(outputTsPath)
        await fs.mkdir(Path.dirname(outputTsPath), {recursive: true})

        await fs.writeFile(outputTsPath, src.tsSource);
    }
}

main()