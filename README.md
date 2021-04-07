## Aquamarine

Aquamarine is a new-gen language for distributed systems.

Aquamarine programs are executed on many peers, sequentially 
or in parallel, forming a single-use coordination network.

Aquamarine's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task.
Therefore, Aquamarine scripts are compiled into several targets at once, with AIR and Typescript as a default.

## Compiler CLI

To compile Aquamarine, clone the repo & build it with `sbt assembly`,
or simply download the latest JAR file from the [releases](https://github.com/fluencelabs/aqua-hll/releases) page.

It requires `java` to run Aquamarine compiler from the command line:

```commandline
java -jar aqua-hll.jar path/to/input/dir path/to/output/dir
```

Input directory should contain files with `aqua` scripts.

## Language overview



## Repository structure