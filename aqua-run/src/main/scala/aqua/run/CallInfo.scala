package aqua.run
import aqua.definitions.FunctionDef

case class CallInfo(
  name: String,
  air: String,
  definitions: FunctionDef,
  config: RunConfig
)

case class RunInfo(
  name: String,
  air: String,
  definitions: FunctionDef
)
