# Installation of Aqua

The easiest way to use Aqua is to download the latest build from npm: [@fluencelabs/aqua](https://www.npmjs.com/package/@fluencelabs/aqua).

```bash
npm i -g @fluencelabs/aqua
aqua --input src/aqua --output src/generated
```

Input directory should contain files with `.aqua` scripts.

Other ways of building Aqua can be found in [INSTALL.md](./INSTALL.md).

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
