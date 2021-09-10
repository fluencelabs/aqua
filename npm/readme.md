## Aqua

Aqua is a new-gen language for distributed systems.

Aqua programs are executed on many peers, sequentially
or in parallel, forming a single-use coordination network.

Aqua's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task.
Therefore, Aqua scripts are compiled into several targets at once, with AIR and Typescript as a default.

## aqua

The package contains a convenience `aqua` wrapper for usage in npm-based projects.

### usage

Get the latest package

```bash
npm i --save-dev @fluencelabs/aqua
```

Create a directory for the source files: `.aqua` and for compiled files: `.ts`

```
mkdir src/aqua src/compiled
```

To compile files run:

```bash
aqua -i ./src/aqua/ -o ./src/compiled
```

Alternatively the compilation script can be put into scripts section of `package.json`

```
...
"scripts": {
    ...
    "compile": "aqua -i ./src/aqua/ -o ./src/compiled"
},
...
```

and can be started with 

```
npm run compile
```

### references

- For the list of compiler options see: https://github.com/fluencelabs/aqua
- To get started writing aqua see: https://github.com/fluencelabs/aqua-playground

