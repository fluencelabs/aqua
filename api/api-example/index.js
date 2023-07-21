//@ts-check

import compile from '@fluencelabs/aqua-api'

// compile call
const compilationResult = await compile({
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
