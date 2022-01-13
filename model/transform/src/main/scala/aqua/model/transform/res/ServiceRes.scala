package aqua.model.transform.res

import aqua.raw.ServiceRaw
import aqua.raw.value.LiteralRaw
import aqua.types.{ArrowType, ScalarType}

// TODO: docs
case class ServiceRes(name: String, members: List[(String, ArrowType)], defaultId: Option[String])

object ServiceRes {

  def fromModel(sm: ServiceRaw): ServiceRes =
    ServiceRes(
      name = sm.name,
      members = sm.arrows.toNel.toList,
      defaultId = sm.defaultId.collect {
        case LiteralRaw(value, t) if ScalarType.string.acceptsValueOf(t) =>
          value
      }
    )
}
