package aqua.run

// `run` command configuration
case class RunConfig(
  consoleServiceId: String = "--console--",
  printFunctionName: String = "print",
  resultName: String = "res",
  functionWrapperName: String = "--someFuncToRun--"
)
