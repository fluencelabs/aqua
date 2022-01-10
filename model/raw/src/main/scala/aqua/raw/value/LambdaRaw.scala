package aqua.raw.value

import aqua.types.Type

sealed trait LambdaRaw {
  def `type`: Type
}
case class IntoFieldRaw(field: String, `type`: Type) extends LambdaRaw
case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends LambdaRaw
