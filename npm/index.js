#!/usr/bin/env node

"use strict";

const { exec } = require("child_process");
const path = require("path");
const fs = require('fs');

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
      ...importArgs,
      ...initArgs,
    ];
}

const argsString = args.join(" ");

console.log("Aqua JS: " + argsString);
exec(argsString, (err, stdout, stderr) => {
  console.error("Aqua JS: " + stderr);
  console.log("Aqua JS: " + stdout);

  if (err) {
    process.exit(err.code);
  }
});
