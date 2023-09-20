import { AquaConfig, Aqua, Call, Input, Path } from "./aqua-api.js";

function getConfig({
  constants = [],
  logLevel = "info",
  noRelay = false,
  noXor = false,
  targetType = "air",
  tracing = false,
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
  );
}

async function compile(...args) {
  try {
    const res = await Aqua.compile(...args);
    return res;
  } catch (error) {
    if (
      typeof error === "object" &&
      "message" in error &&
      typeof error.message === "string"
    ) {
      throw new Error(error.message);
    }
    throw error;
  }
}

export function compileFromString({ code, imports = [], ...commonArgs }) {
  return compile(new Input(code), imports, getConfig(commonArgs));
}

export function compileFromPath({ filePath, imports = [], ...commonArgs }) {
  return compile(new Path(filePath), imports, getConfig(commonArgs));
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
    imports,
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
    imports,
    getConfig(commonArgs),
  );
}
