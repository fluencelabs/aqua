package aqua.model.inline

import aqua.raw.value.{CallArrowRaw, LiteralRaw, ValueRaw}
import aqua.types.{ArrowType, ProductType, ScalarType}

object RawBuilder {

  def add(l: ValueRaw, r: ValueRaw): ValueRaw =
    CallArrowRaw.service(
      abilityName = "math",
      serviceId = LiteralRaw.quote("math"),
      funcName = "add",
      baseType = ArrowType(
        ProductType(List(ScalarType.i64, ScalarType.i64)),
        ProductType(
          List(l.`type` `∪` r.`type`)
        )
      ),
      arguments = List(l, r)
    )
}
