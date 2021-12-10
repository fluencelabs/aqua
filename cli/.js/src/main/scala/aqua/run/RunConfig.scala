package aqua.run

import aqua.builder.ArgumentGetter
import scribe.Level

// `run` command configuration
case class RunConfig(
  timeout: Int,
  logLevel: Level,
  printAir: Boolean,
  secretKey: Option[Array[Byte]],
  argumentGetters: Map[String, ArgumentGetter],
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "print-and-stop",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
