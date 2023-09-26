package aqua.raw.arrow

import aqua.raw.value.ValueRaw
import aqua.raw.RawPart
import aqua.types.Type

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = arrow.`type`

  def capturedVars: Set[String] = {
    val freeBodyVars = arrow.body.usesVarNames.value
    val argsNames = arrow.`type`.domain
      .toLabelledList()
      .map { case (name, _) => name }
      .toSet

    freeBodyVars -- argsNames
  }
}
