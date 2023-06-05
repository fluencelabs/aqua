package aqua.api
import aqua.model.transform.TransformConfig

enum TargetType:
  case TypeScriptType, JavaScriptType, AirType

case class AquaAPIConfig(
  targetType: TargetType = TargetType.AirType,
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false
) {

  def getTransformConfig: TransformConfig =
    if (noRelay) TransformConfig(relayVarName = None, wrapWithXor = !noXor)
    else TransformConfig(wrapWithXor = !noXor)
}
