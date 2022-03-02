package aqua.raw.value

import aqua.types.Type

sealed trait LambdaRaw {
  def `type`: Type

  def map(f: ValueRaw => ValueRaw): LambdaRaw

  def renameVars(vals: Map[String, String]): LambdaRaw = this

}

case class IntoFieldRaw(field: String, `type`: Type) extends LambdaRaw {
  override def map(f: ValueRaw => ValueRaw): LambdaRaw = this
}

case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends LambdaRaw {

  override def map(f: ValueRaw => ValueRaw): LambdaRaw = IntoIndexRaw(f(idx), `type`)

  override def renameVars(vals: Map[String, String]): LambdaRaw =
    IntoIndexRaw(idx.renameVars(vals), `type`)
}
