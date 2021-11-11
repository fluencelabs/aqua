package aqua.run

case class RunConfig(
  consoleServiceId: String = "--console--",
  printFunction: String = "print",
  resultName: String = "res",
  wrapFunctionName: String = "--someFuncToRun--"
)
