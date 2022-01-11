package aqua.raw.value

import aqua.types.Type

sealed trait LambdaRaw {
  def `type`: Type

  def usesVarNames: Set[String] = Set.empty

  def resolveWith(map: Map[String, ValueRaw]): LambdaRaw = this

  def renameVars(map: Map[String, String]): LambdaRaw = this

}

case class IntoFieldRaw(field: String, `type`: Type) extends LambdaRaw

case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends LambdaRaw {
  override def usesVarNames: Set[String] = idx.usesVarNames

  override def resolveWith(map: Map[String, ValueRaw]): LambdaRaw =
    IntoIndexRaw(idx.resolveWith(map), `type`)

  override def renameVars(map: Map[String, String]): LambdaRaw =
    IntoIndexRaw(idx.renameVars(map), `type`)
}
