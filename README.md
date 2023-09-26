# Aqua

[![release](https://github.com/fluencelabs/aqua/actions/workflows/release.yml/badge.svg)](https://github.com/fluencelabs/aqua/actions/workflows/release.yml)
[![npm](https://img.shields.io/npm/v/@fluencelabs/aqua-api)](https://www.npmjs.com/package/@fluencelabs/aqua-api)

[Aqua](https://fluence.dev/docs/aqua-book/introduction) is an open-source language for distributed workflow coordination in p2p networks. Aqua programs are executed on many peers, sequentially or in parallel, forming a single-use coordination network. Applications are turned into hostless workflows over distributed function calls, which enables various levels of decentralization: from handling by a limited set of servers to complete peer-to-peer architecture by connecting user devices directly. Aqua is the core of the [Fluence](https://fluence.network/) protocol and a framework for internet or private cloud applications.


## Usage

The easiest way to use Aqua is through [Fluence CLI](https://github.com/fluencelabs/cli) with [aqua command](https://github.com/fluencelabs/cli/blob/main/docs/commands/README.md#fluence-aqua).

Other ways of using Aqua are described in [USAGE.md](./USAGE.md).


## Documentation

Comprehensive documentation and usage examples as well as a number of videos can be found in [Aqua Book](https://fluence.dev/docs/aqua-book/introduction). [Aqua Playground](https://github.com/fluencelabs/aqua-playground) demonstrates how to start writing Aqua and integrate it into a [TypeScript](https://www.typescriptlang.org/) application. Numerous videos are available at our [YouTube channel](https://www.youtube.com/@fluencelabs).


## Repository Structure

- [**api**](./api) - Aqua API for JS
    - [api](./api/api) - Scala sources
    - [api-npm](./api/api-npm) - JS project
    - [api-example](./api/api-example) - usage example
- [**aqua-run**](./aqua-run) - Aqua API to run functions
- [**backend**](./backend) - compilation backend interface
    - [backend/air](./backend/air) - generates AIR code from the middle-end model
    - [backend/ts](./backend/ts) - generates AIR code and TypeScript wrappers for use with [Fluence JS]( https://github.com/fluencelabs/fluence-js) SDK
- [**compiler**](./compiler) - compiler as a pure function made from _linker_, _semantics_ and _backend_
- [**model**](./model) - middle-end, internal representation of the code, optimizations and transformations
    - [transform](./model/transform) - optimizations and transformations, converting model to the result, ready to be rendered
    - [test-kit](./model/test-kit) - tests and test helpers for the model and transformations
- [**linker**](./linker) - checks dependencies between modules, builds and combines an abstract dependencies tree
- [**parser**](./parser) - parser, takes source text and produces a source AST
- [**semantics**](./semantics) - rules to convert source AST into the model
- [**types**](./types) - data types, arrows, stream types definitions and variance


## Support

Please, file an [issue](https://github.com/fluencelabs/aqua/issues) if you find a bug. You can also contact us at [Discord](https://discord.com/invite/5qSnPZKh7u) or [Telegram](https://t.me/fluence_project).  We will do our best to resolve the issue ASAP.


## Contributing

Any interested person is welcome to contribute to the project. Please, make sure you read and follow some basic [rules](./CONTRIBUTING.md).


## License

All software code is copyright (c) Fluence Labs, Inc. under the [Apache-2.0](./LICENSE) license.

