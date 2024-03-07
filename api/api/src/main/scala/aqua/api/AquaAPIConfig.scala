package aqua.api

import aqua.compiler.AquaCompilerConf
import aqua.model.transform.TransformConfig

enum TargetType:
  case TypeScriptType, JavaScriptType, AirType

case class AquaAPIConfig(
  targetType: TargetType = TargetType.AirType,
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false,
  tracing: Boolean = false,
  noEmptyResponse: Boolean = true
) {

  def getTransformConfig: TransformConfig = {
    val config = TransformConfig(
      tracing = Option.when(tracing)(TransformConfig.TracingConfig.default),
      noEmptyResponse = noEmptyResponse,
      noXor = noXor
    )

    if (noRelay) config.copy(relayVarName = None)
    else config
  }

  def getCompilerConfig: AquaCompilerConf = {
    val config = AquaCompilerConf()

    if (noRelay) config.copy(relayVarName = None)
    else config
  }
}
