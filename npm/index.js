#!/usr/bin/env node

"use strict";

import { exec } from "child_process";
import * as path from 'path';
import fs from 'fs';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const nm = path.join("./", "node_modules")
let initArgs = process.argv.slice(2)

let args = [];
if ((initArgs.includes('-v') || initArgs.includes('--version'))) {
    args = [
      "node",
      path.join(__dirname, "aqua.js"),
      "--version",
    ];
} else {
    let importArgs = []
    if (fs.existsSync(nm) && fs.lstatSync(nm).isDirectory()) {
      importArgs = ["-m", "node_modules"]
    }
    args = [
      "node",
      path.join(__dirname, "aqua.js"),
      ...initArgs,
      ...importArgs,
    ];
}

const argsString = args.join(" ");

console.log("Aqua: " + argsString);
exec(argsString, (err, stdout, stderr) => {
  console.error("Aqua: " + stderr);
  console.log("Aqua: " + stdout);

  if (err) {
    process.exit(err.code);
  }
});
