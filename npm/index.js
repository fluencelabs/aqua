#!/usr/bin/env node

"use strict";

import "./aqua.js";
import * as fs from "fs"

import { createRequire } from 'module';
console.log(import.meta)
console.dir(import.meta)
console.log(JSON.stringify(import.meta))
const require = createRequire(import.meta.url);

const builtinPath = require.resolve("@fluencelabs/aqua-lib/builtin.aqua");
