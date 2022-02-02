## Installation

- run `sbt "cliJS/fullLinkJS"` in a root dir of a project after any code update (better to keep `sbt` running for faster compilation) 
- `npm` installed required
- run `npm i` in `npm` directory
- feel free to change `@fluencelabs/fluence` version in `package.json` file, run `npm i` after changes

## Run script

Generally, use this script to run compiled aqua compiler from a repo: 

```
npm run from:scalajs -- run -f "someFunction(arg1, arg2)" -i path/to/aqua --data-path path/to/args -m path/to/exports --addr /multiaddr/to/node  
```

- `-f or --func` is a function to call with arguments
- `-i or --input` aqua file where function located
- `-m or --import` imports location, could be used multiple times
- `-a or --addr` multiaddress to a Fluence node
- `-p or --data-path` path to a file with JSON where arguments are located

As example, use `test/sample.aqua` with args from `test/data.json` running on `/dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi` node:

```
npm run from:scalajs -- run -f "identityArgsAndReturn(structField, stringField, numberField)" -i test/sample.aqua --data-path test/data.json --addr /dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi
```

To simplify experience you can use `./aqua-run.sh` command and change all arguments straight in this file.

## Node addresses

Different Fluence network addresses could be found here: https://github.com/fluencelabs/fluence-network-environment/blob/main/src/index.ts

## Useful flags
- `--print-air` to print resulted air
- `--no-xor` to disable xor wrapping aroung service calls
- `--sk secret_key` send request signed with specific secret key. Secret key could be created with `npm run from:scalajs -- create_keypair` or `aqua create_keypair` if you want to use installed aqua
- `--data "json""` use instead of `--data-path` to pass arguments through command line
- `--timeout 10000` to change timeout
- `--log-level debug/info/..` to change log level

## Builtins for `aqua run`

You can find all builtins in aqua/run-builtins/run-builtins.aqua

