package aqua.raw

import aqua.raw.value.ValueRaw

case class ConstantRaw(name: String, value: ValueRaw, allowOverrides: Boolean) extends Raw
