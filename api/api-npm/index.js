// @ts-check
const { AquaConfig, Aqua, Call, Input, Path } = require("./aqua-api.js");

async function compile({
  funcCall,
  code,
  filePath,
  data = {},
  imports = [],
  constants = [],
  logLevel = "info",
  noRelay = false,
  noXor = false,
  targetType = "air",
  tracing = false,
}) {
  const config = new AquaConfig(
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

  if (typeof funcCall === "string" && filePath !== undefined) {
    const result = await Aqua.compile(
      new Call(funcCall, data, new Input(filePath)),
      imports,
      config,
    );

    return result;
  }

  if (typeof code === "string") {
    return Aqua.compile(new Input(code), imports, config);
  }

  return Aqua.compile(new Path(filePath), imports, config);
}

module.exports = compile;
