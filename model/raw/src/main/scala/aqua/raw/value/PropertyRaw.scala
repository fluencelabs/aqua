package aqua.raw.value

import aqua.types.{StructType, Type}
import cats.data.NonEmptyMap

sealed trait PropertyRaw {
  def `type`: Type

  def map(f: ValueRaw => ValueRaw): PropertyRaw

  def renameVars(vals: Map[String, String]): PropertyRaw = this

  def varNames: Set[String]
}

case class IntoFieldRaw(name: String, `type`: Type) extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): PropertyRaw = this

  override def varNames: Set[String] = Set.empty
}

case class IntoArrowRaw(name: String, arrowType: Type, arguments: List[ValueRaw])
    extends PropertyRaw {

  override def `type`: Type = arrowType

  override def map(f: ValueRaw => ValueRaw): PropertyRaw = this

  override def varNames: Set[String] = arguments.flatMap(_.varNames).toSet

  override def renameVars(vals: Map[String, String]): PropertyRaw =
    copy(arguments = arguments.map(_.renameVars(vals)))
}

case class IntoCopyRaw(`type`: StructType, fields: NonEmptyMap[String, ValueRaw])
    extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): IntoCopyRaw = copy(fields = fields.map(f))

  override def varNames: Set[String] = Set.empty

  override def renameVars(vals: Map[String, String]): IntoCopyRaw = this
}

case class FunctorRaw(name: String, `type`: Type) extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): FunctorRaw = this

  override def renameVars(vals: Map[String, String]): FunctorRaw = this

  override def varNames: Set[String] = Set.empty
}

case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends PropertyRaw {

  override def map(f: ValueRaw => ValueRaw): IntoIndexRaw = IntoIndexRaw(f(idx), `type`)

  override def renameVars(vals: Map[String, String]): IntoIndexRaw =
    IntoIndexRaw(idx.renameVars(vals), `type`)

  override def varNames: Set[String] = idx.varNames
}
