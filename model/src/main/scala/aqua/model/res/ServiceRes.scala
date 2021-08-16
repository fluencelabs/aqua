package aqua.model.res

import aqua.model.ServiceModel
import aqua.types.{ArrowType, ScalarType}
import aqua.model.LiteralModel

case class ServiceRes(name: String, members: List[(String, ArrowType)], defaultId: Option[String])

object ServiceRes {

  def fromModel(sm: ServiceModel): ServiceRes =
    ServiceRes(
      name = sm.name,
      members = sm.arrows.toNel.toList,
      defaultId = sm.defaultId.collect {
        case LiteralModel(value, t) if ScalarType.string.acceptsValueOf(t) =>
          value
      }
    )
}
