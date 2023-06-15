# Aqua playground

Repository demonstrating the how to start writing aqua and integrate it into a typescript application

## Getting started

Install dependencies

```
npm install
```

To compile the aqua files execute

```
npm run compile-aqua
```

To start the `index.ts` execute

```
npm run run
```

To run all examples in `example` directory
```
npm run exec
```

If everything works correctly logs without errors will be printed on a screen:

Then you can add or modify `aqua` files in `aqua` directory, these files will be compiled into `/src/compiled` and you can use it in a TypeScript in your preferable way.

## Project structure

Aqua source files are located in `src/aqua`directory.

Aqua files are compiled into .ts located in `/src/compiled` directory.

Entry point to use aqua compiled files from TypeScript: `/src/index.ts`

Code that call all examples and check results: `/src/run-examples.ts`

## References

Documentation for the compiler can be found in the official repo: https://github.com/fluencelabs/aqua