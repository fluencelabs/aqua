package aqua.raw

import aqua.raw.arrow.FuncRaw
import aqua.types.{ArrowType, StructType, Type}
import cats.data.NonEmptyMap
import aqua.raw.value.ValueRaw

import scala.collection.immutable.SortedMap

case class ScopeRaw(
  name: String,
  fieldsAndArrows: NonEmptyMap[String, Type]
) extends RawPart {
  lazy val rawPartType: StructType = StructType(name, fieldsAndArrows)


  override def rename(s: String): RawPart = copy(name = s)

}
