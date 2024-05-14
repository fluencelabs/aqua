package aqua.raw.arrow

import aqua.raw.RawPart
import aqua.types.Type

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  def addAbilityName(s: String): RawPart = copy(arrow = arrow.copy(`type` = Type.addAbilityNameArrow(s, arrow.`type`)))

  override def rawPartType: Type = arrow.`type`

  // vars that we capture from external space (outer functions, etc)
  lazy val capturedVars: Set[String] = {
    val freeBodyVars: Set[String] = arrow.body.usesVarNames.value
    val argsNames = arrow.`type`.domain
      .toLabelledList()
      .map { case (name, _) => name }
      .toSet

    freeBodyVars -- argsNames
  }
}
