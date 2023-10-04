package aqua.model.inline

import aqua.raw.value.{CallArrowRaw, CallServiceRaw, LiteralRaw, ValueRaw}
import aqua.types.{ArrowType, ProductType, ScalarType}

object RawBuilder {

  def add(l: ValueRaw, r: ValueRaw): ValueRaw =
    CallServiceRaw(
      serviceId = LiteralRaw.quote("math"),
      fnName = "add",
      baseType = ArrowType(
        ProductType(List(ScalarType.i64, ScalarType.i64)),
        ProductType(
          List(l.`type` `∪` r.`type`)
        )
      ),
      arguments = List(l, r)
    )
}
