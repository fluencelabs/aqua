package aqua.model.inline

import aqua.raw.value.{ApplyBinaryOpRaw, ValueRaw}
import aqua.types.{ArrowType, ProductType, ScalarType}

object RawBuilder {

  def add(l: ValueRaw, r: ValueRaw): ValueRaw =
    ApplyBinaryOpRaw.Add(l, r)
}
