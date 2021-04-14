#!/usr/bin/env node

"use strict";

const { exec } = require("child_process");
const path = require("path");

const args = [
  "java",
  "-jar",
  path.join(__dirname, "\\aqua-cli.jar"),
  "-m",
  "node_modules",
  ...process.argv.slice(2),
];

const argsString = args.join(" ");

console.log(argsString);
exec(argsString, (err, stdout, stderr) => {
  console.error(stderr);
  console.log(stdout);

  if (err) {
    process.exit(err.code);
  }
});
