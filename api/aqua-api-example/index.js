import {
    Aqua,
    Call,
    Path,
} from "@fluencelabs/aqua-api/aqua-api.js";
import { FluencePeer } from "@fluencelabs/fluence";
import { callFunctionImpl } from "@fluencelabs/fluence/dist/internal/compilerSupport/v3impl/callFunction.js";

const aquaPath = new Path("test.aqua")
// write function that we want to call and arguments
const args = {num: 42}
const call = new Call("getNumber(num)", args, aquaPath)

// compile call
const compilationResult = await Aqua.compile(call, [])
// get function definition and AIR script
const {funcDef, script} = compilationResult.functionCall

// initialize fluence client
const fluence = new FluencePeer();
await fluence.start({})

// make a call with arguments
const result = await callFunctionImpl(funcDef, script, {}, fluence, args)

// print a result
console.log(result)

// stop fluence client
await fluence.stop()