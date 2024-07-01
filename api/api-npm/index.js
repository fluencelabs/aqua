/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import { AquaConfig, Aqua, Call, Input, Path } from "./aqua-api.js";

function getConfig({
  constants = [],
  logLevel = "info",
  noRelay = false,
  noXor = false,
  targetType = "air",
  tracing = false,
  noEmptyResponse = true,
}) {
  return new AquaConfig(
    logLevel,
    constants,
    noXor,
    noRelay,
    {
      ts: "typescript",
      js: "javascript",
      air: "air",
    }[targetType],
    tracing,
    noEmptyResponse,
  );
}

function normalizeImports(imports) {
  if (imports === undefined || imports === null) {
    return {}; // No imports
  }

  if (Array.isArray(imports)) {
    return {
      "/": {
        "": imports,
      },
    };
  }

  // Transform each inner string into an array
  return Object.fromEntries(
    Object.entries(imports).map(([pathPrefix, info]) => [
      pathPrefix,
      Object.fromEntries(
        Object.entries(info).map(([importPrefix, locations]) => [
          importPrefix,
          Array.isArray(locations) ? locations : [locations],
        ]),
      ),
    ]),
  );
}

async function compile(...args) {
  try {
    const res = await Aqua.compile(...args);
    return res;
  } catch (error) {
    if (
      typeof error === "object" &&
      error !== null &&
      "message" in error &&
      typeof error.message === "string"
    ) {
      throw new Error(error.message);
    }
    throw error;
  }
}

export function compileFromString({ code, imports = [], ...commonArgs }) {
  return compile(
    new Input(code),
    normalizeImports(imports),
    getConfig(commonArgs),
  );
}

export function compileFromPath({ filePath, imports = [], ...commonArgs }) {
  return compile(
    new Path(filePath),
    normalizeImports(imports),
    getConfig(commonArgs),
  );
}

export function compileAquaCallFromString({
  code,
  funcCall,
  data,
  imports = [],
  ...commonArgs
}) {
  return compile(
    new Call(funcCall, data, new Input(code)),
    normalizeImports(imports),
    getConfig(commonArgs),
  );
}

export function compileAquaCallFromPath({
  filePath,
  funcCall,
  data,
  imports = [],
  ...commonArgs
}) {
  return compile(
    new Call(funcCall, data, new Input(filePath)),
    normalizeImports(imports),
    getConfig(commonArgs),
  );
}
