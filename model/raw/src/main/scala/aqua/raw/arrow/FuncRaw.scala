package aqua.raw.arrow

import aqua.raw.RawPart
import aqua.types.Type

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = arrow.`type`

  // all vars that we use inside body
  lazy val freeBodyVars: Set[String] = arrow.body.usesVarNames.value

    // vars that we capture from external space (outer functions, etc)
  lazy val varsWithoutArgs: Set[String] = {
    val argsNames = arrow.`type`.domain
      .toLabelledList()
      .map { case (name, _) => name }
      .toSet

    freeBodyVars -- argsNames
  }
}
