package aqua.run

import aqua.builder.{ArgumentGetter, ServiceFunction}
import scribe.Level

// `run` command configuration
case class RunConfig(
  timeout: Int,
  logLevel: Level,
  printAir: Boolean,
  secretKey: Option[Array[Byte]],
  argumentGetters: Map[String, ArgumentGetter],
  services: List[ServiceFunction] = Nil,
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "console-log",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
