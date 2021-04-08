## Aquamarine

Aquamarine is a new-gen language for distributed systems.

Aquamarine programs are executed on many peers, sequentially 
or in parallel, forming a single-use coordination network.

Aquamarine's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task.
Therefore, Aquamarine scripts are compiled into several targets at once, with AIR and Typescript as a default.

## Compiler CLI

To build the Aquamarine compiler, clone the repo & run `sbt assembly`,
or simply download the latest JAR file from the [releases](https://github.com/fluencelabs/aqua-hll/releases) page.

It requires `java` to run Aquamarine compiler from the command line:

```commandline
java -jar aqua-hll.jar path/to/input/dir path/to/output/dir
```

Input directory should contain files with `aqua` scripts.

## Repository structure

- **[types](./types)** – data types, arrows, stream types definitions and variance
- **[parser](./parser)** - parser, takes source text and produces a source AST
- **[model](./model)** - middle-end, internal representation of the code, optimizations and transfromations
- **[semantics](./semantics)** - rules to convert source AST into the model
- **[backend/air](./backend/air)** – generates AIR code from the middle-end model
- **[backend/ts](./backend/ts)** - generates AIR code and Typescript wrappers for use with Fluence JS SDK
- **[cli](./cli)** - CLI interface
