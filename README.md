## Aqua

[![release](https://github.com/fluencelabs/aqua/actions/workflows/release.yml/badge.svg)](https://github.com/fluencelabs/aqua/actions/workflows/release.yml)

Aqua is a new-gen language for distributed systems.

Aqua programs are executed on many peers, sequentially 
or in parallel, forming a single-use coordination network.

Aqua's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task.
Therefore, Aqua scripts are compiled into several targets at once, with AIR and Typescript as a default.

Please refer to [Aqua Book](https://doc.fluence.dev/aqua-book/) to learn more about Aqua language.

## Install and run

The easiest way to use Aqua is to download the latest build from npm: [@fluencelabs/aqua](https://www.npmjs.com/package/@fluencelabs/aqua).

```bash
npm i -g @fluencelabs/aqua
aqua --input src/aqua --output src/generated
```

Input directory should contain files with `.aqua` scripts.

## Build from sources

If you want to build Aqua compiler from the sourcecode, you need [Scala](https://www.scala-lang.org/)'s `sbt` installed.

Aqua compiler itself can be compiled to and distributed either as JavaScript or Java file.

### Build to JS

Run `sbt cliJS/fullLinkOpt` to build JavaScript file. You can find the compiled file in: `cli/.js/target/scala-%scala-version%/cli-opt`.

Then run it with `node`:

```bash
node aqua-%version_number%.js -i path/to/input/dir -o path/to/output/dir
```

Javascript build is the default for Aqua.

### Build to JVM

Run `sbt cli/assembly` to build JAR file. It is located in `cli/.jvm/target/scala-%scala-version%/`

It requires `java` to run Aqua compiler in `.jar` file from the command line:

```bash
java -jar aqua-%version_number%.jar -i path/to/input/dir -o path/to/output/dir
```

## Repository structure

- **[types](./types)** – data types, arrows, stream types definitions and variance
- **[parser](./parser)** - parser, takes source text and produces a source AST
- **[model](./model)** - middle-end, internal representation of the code, optimizations and transfromations
- **[model/transform](./model/transform)** - optimizations and transfromations, converting model to the result, ready to be rendered
- **[model/test-kit](./model/test-kit)** - tests and test helpers for the model and transformations
- **[semantics](./semantics)** - rules to convert source AST into the model
- **[linker](./linker)** - checks dependencies between modules, builds and combines an abstract dependencies tree
- **[backend](./backend)** - compilation backend interface
- **[compiler](./compiler)** - compiler as a pure function made from _linker_, _semantics_ and _backend_
- **[backend/air](./backend/air)** – generates AIR code from the middle-end model
- **[backend/ts](./backend/ts)** - generates AIR code and Typescript wrappers for use with Fluence JS SDK
- **[cli](./cli)** - CLI interface

## References

- [Aqua Book](https://doc.fluence.dev/aqua-book/)
- [Aqua Changelog](https://doc.fluence.dev/aqua-book/changelog)
- [Fluence docs](https://fluence.dev/)
- [Examples & tutorials](https://github.com/fluencelabs/examples)
- [Aqua Playground](https://github.com/fluencelabs/aqua-playground)