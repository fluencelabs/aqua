## Aquamarine HLL

The high level language that compiles to AIR and some wrappers in the host language (e.g. TypeScript).

## Build and use jar file
To build a file use this command:

```commandline
sbt assembly
```


jar file will be in `target/scala-2.13/aqua-hll.jar`

Run:

```commandline
java -jar aqua-hll.jar path/to/input/dir path/to/output/dir
```
input directory should contain files with `aqua` scripts

### TODO

- Lambda
- Build data types: extend
- Platform-specific Predef with Return ability
- Implementation for abilities
- Abilities passing
- Print syntax errors better way
- For the offset, find a token
