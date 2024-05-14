package aqua.raw

import aqua.types.{BottomType, Type}

// Part for structures that have errors inside but must be registered in context
case class ErroredPart(name: String) extends RawPart {
  override def rawPartType: Type = BottomType

  override def rename(s: String): RawPart = copy(name = s)

  def addAbilityName(s: String): RawPart = this
}
