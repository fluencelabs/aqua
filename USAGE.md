# Usage of Aqua

## Fluence CLI

The easiest way to use Aqua is through [Fluence CLI](https://github.com/fluencelabs/cli) with [aqua command](https://github.com/fluencelabs/cli/blob/main/docs/commands/README.md#fluence-aqua).

## JS API

You can use Aqua compiler API directly from JS code by installing [`@fluencelabs/aqua-api` package](https://www.npmjs.com/package/@fluencelabs/aqua-api). See an example usage in [api/api-example](./api/api-example).

## Build from sources

If you want to build Aqua compiler API from the source code, you need [Scala](https://www.scala-lang.org/)'s [`sbt`](https://www.scala-sbt.org/) installed.

### Build to JS package

Javascript build is the default for Aqua compiler API.

Run `sbt "aqua-apiJS/fullLinkJS"`. It will generate JS package in `api/api-npm` directory.


### Build to JVM library

Building Aqua compiler API as JVM lib is technically possible, but is not supported.
