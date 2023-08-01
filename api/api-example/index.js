//@ts-check

import { compileAquaCallFromPath } from '@fluencelabs/aqua-api'

// compile call
const compilationResult = await compileAquaCallFromPath({
  filePath: 'test.aqua',
  data: { num: 3 },
  funcCall: 'getNumber(num)',
})

const {
  errors,
  functionCall: { funcDef, script },
  functions,
  generatedSources,
  services,
} = compilationResult

console.log(script)
