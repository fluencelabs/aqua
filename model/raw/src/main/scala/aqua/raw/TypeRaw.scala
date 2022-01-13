package aqua.raw

import aqua.types.Type

case class TypeRaw(name: String, `type`: Type) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = `type`
}
