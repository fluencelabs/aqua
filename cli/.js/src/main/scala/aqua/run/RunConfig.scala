package aqua.run

import aqua.builder.GetterBuilder
import scribe.Level

// `run` command configuration
case class RunConfig(
  timeout: Int,
  logLevel: Level,
  printAir: Boolean,
  secretKey: Option[Array[Byte]],
  argumentGetters: Map[String, GetterBuilder],
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "print-and-stop",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
