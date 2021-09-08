## Aqua

[![release](https://github.com/fluencelabs/aqua/actions/workflows/release.yml/badge.svg)](https://github.com/fluencelabs/aqua/actions/workflows/release.yml)

Aqua is a new-gen language for distributed systems.

Aqua programs are executed on many peers, sequentially 
or in parallel, forming a single-use coordination network.

Aqua's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task.
Therefore, Aqua scripts are compiled into several targets at once, with AIR and Typescript as a default.

## Using Aqua

Please refer to [Aqua Book](https://doc.fluence.dev/aqua-book/) to learn how to use Aqua.

## Compiler CLI

To build the Aqua compiler, clone the repo & run `sbt cliJS/fullLinkOpt` to build JavaScript file. File location: `cli/.js/target/scala-%scala-version%/cli-opt`.
Or `sbt cli/assembly` to build JAR file. File location: `cli/.jvm/target/scala-%scala-version%/`
Or simply download the latest JS or JAR file from the [releases](https://github.com/fluencelabs/aqua/releases) page.

It requires `node` to run Aqua compiler in `.js` file from the command line:

```commandline
node aqua-%version_number%.js -i path/to/input/dir -o path/to/output/dir
```

It requires `java` to run Aqua compiler in `.jar` file from the command line:

```commandline
java -jar aqua-%version_number%.jar -i path/to/input/dir -o path/to/output/dir
```

Input directory should contain files with `aqua` scripts.

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
