package aqua.model.inline

import aqua.raw.value.{CallArrowRaw, LiteralRaw, ValueRaw}
import aqua.types.{ArrowType, ProductType}

object RawBuilder {

  def add(l: ValueRaw, r: ValueRaw): ValueRaw =
    CallArrowRaw(
      ability = Some("math"),
      name = "add",
      arguments = List(l, r),
      baseType = ArrowType(
        ProductType(List(l.`type`, r.`type`)),
        ProductType(
          List(l.`type` `∪` r.`type`)
        )
      ),
      serviceId = Some(LiteralRaw.quote("math"))
    )
}
