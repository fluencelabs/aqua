package aqua.raw

import aqua.types.{ArrowType, StructType}
import cats.data.NonEmptyMap
import aqua.raw.value.ValueRaw

case class ServiceRaw(
  name: String,
  arrows: NonEmptyMap[String, ArrowType],
  defaultId: Option[ValueRaw]
) extends Raw {
  def `type`: StructType = StructType(name, arrows)
}
