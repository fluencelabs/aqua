## Installation

- run `sbt "cliJS/fullLinkJS"` in a root dir of a project after any code update (better to keep `sbt` running for faster compilation) 
- `npm` installed required
- run `npm i` in `npm` directory
- feel free to change `@fluencelabs/fluence` version in `package.json` file, run `npm i` after changes

## Run script

Generally, use this script to run aqua: 

```
npm run from:scalajs -- run -f "someFunction(arg1, arg2)" -i path/to/aqua --data-path path/to/args -m path/to/exports --addr /multiaddr/to/node  
```

As example, use `test/sample.aqua` with args from `test/data.json` running on `/dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi` node:

```
npm run from:scalajs -- run -f "identityArgsAndReturn(structField, stringField, numberField)" -i test/sample.aqua --data-path test/data.json --addr /dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi
```

Different Fluence network addresses could be found here: https://github.com/fluencelabs/fluence-network-environment/blob/main/src/index.ts