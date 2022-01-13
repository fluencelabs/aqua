package aqua.raw

import aqua.raw.value.ValueRaw
import aqua.types.Type

case class ConstantRaw(name: String, value: ValueRaw, allowOverrides: Boolean) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = value.lastType
}
