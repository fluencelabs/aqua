package aqua.api
import aqua.model.transform.TransformConfig

sealed trait TargetType
case object TypeScriptType extends TargetType
case object JavaScriptType extends TargetType
case object AirType extends TargetType

case class AquaAPIConfig(
  targetType: TargetType = AirType,
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false
) {

  def getTransformConfig: TransformConfig =
    if (noRelay) TransformConfig(relayVarName = None, wrapWithXor = !noXor)
    else TransformConfig(wrapWithXor = !noXor)
}
