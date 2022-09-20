package aqua.raw.value

import aqua.types.Type

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

case class FunctorRaw(name: String, `type`: Type) extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): PropertyRaw = this

  override def varNames: Set[String] = Set.empty
}

case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends PropertyRaw {

  override def map(f: ValueRaw => ValueRaw): PropertyRaw = IntoIndexRaw(f(idx), `type`)

  override def renameVars(vals: Map[String, String]): PropertyRaw =
    IntoIndexRaw(idx.renameVars(vals), `type`)

  override def varNames: Set[String] = idx.varNames
}
