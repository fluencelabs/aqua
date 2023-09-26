package aqua.api
import aqua.model.transform.TransformConfig

enum TargetType:
  case TypeScriptType, JavaScriptType, AirType

case class AquaAPIConfig(
  targetType: TargetType = TargetType.AirType,
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false, // TODO: Remove
  noRelay: Boolean = false,
  tracing: Boolean = false
) {

  def getTransformConfig: TransformConfig = {
    val config = TransformConfig(
      tracing = Option.when(tracing)(TransformConfig.TracingConfig.default)
    )

    if (noRelay) config.copy(relayVarName = None)
    else config
  }
}
