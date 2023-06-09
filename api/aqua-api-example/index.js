import {
    Aqua,
    Call,
    Path,
    AquaConfig
} from "@fluencelabs/aqua-api/aqua-api.js";

const aquaPath = new Path("test.aqua")
// write function that we want to call and arguments
const args = {num: 42}
const call = new Call("getNumber(num)", args, aquaPath)


const inputPath = new Path("test.aqua")

// compile call
const compilationResult = await Aqua.compile(inputPath, [], new AquaConfig("info", [], false, false, "typescript"))


/*
// Compilation result definition
export class CompilationResult {
    // List of service definitions to register in Fluence JS Client
    services: Record<string, ServiceDef>
    // List of function definitions to call in Fluence JS Client
    functions: Record<string, AquaFunction>
    // Definition of wrapped function to call in Fluence JS Client
    functionCall?: AquaFunction
    // List of errors. All other fields will be empty if `errors` not empty
    errors: string[]
}
 */

// get function definition, that describes types of arguments and results of a function
// and AIR script

console.log(compilationResult.generatedSources)
