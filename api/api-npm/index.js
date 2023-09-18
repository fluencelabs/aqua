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

export function compileFromString({ code, ...commonArgs }) {
  const config = getConfig(commonArgs);
  const { imports = [] } = commonArgs;
  return Aqua.compile(new Input(code), imports, config);
}

export function compileFromPath({ filePath, ...commonArgs }) {
  const config = getConfig(commonArgs);
  const { imports = [] } = commonArgs;
  return Aqua.compile(new Path(filePath), imports, config);
}

export function compileAquaCallFromString({
  code,
  funcCall,
  data,
  ...commonArgs
}) {
  const config = getConfig(commonArgs);
  const { imports = [] } = commonArgs;
  return Aqua.compile(
    new Call(funcCall, data, new Input(code)),
    imports,
    config,
  );
}

export function compileAquaCallFromPath({
  filePath,
  funcCall,
  data,
  ...commonArgs
}) {
  const config = getConfig(commonArgs);
  const { imports = [] } = commonArgs;
  return Aqua.compile(
    new Call(funcCall, data, new Input(filePath)),
    imports,
    config,
  );
}
