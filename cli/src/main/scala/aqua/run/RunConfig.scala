package aqua.run

// `run` command configuration
case class RunConfig(
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "print-and-stop",
  resultName: String = "res",
  functionWrapperName: String = "--someFuncToRun--"
)
