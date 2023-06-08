package aqua.model.inline

import aqua.raw.value.{CallArrowRaw, LiteralRaw, ValueRaw}
import aqua.types.{ArrowType, ProductType, ScalarType}

object RawBuilder {

  def add(l: ValueRaw, r: ValueRaw): ValueRaw =
    CallArrowRaw(
      ability = Some("math"),
      name = "add",
      arguments = List(l, r),
      baseType = ArrowType(
        ProductType(List(ScalarType.i64, ScalarType.i64)),
        ProductType(
          List(l.`type` `∪` r.`type`)
        )
      ),
      serviceId = Some(LiteralRaw.quote("math"))
    )
}
