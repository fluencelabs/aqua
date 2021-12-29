package aqua.run

import aqua.builder.{ArgumentGetter, ServiceFunction}
import aqua.GeneralRunOptions
import scribe.Level

// `run` command configuration
case class RunConfig(
  common: GeneralRunOptions,
  // services that will pass arguments to air
  argumentGetters: Map[String, ArgumentGetter],
  // services that will be used in aqua code and need to be registered
  services: List[ServiceFunction] = Nil,
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "console-log",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
