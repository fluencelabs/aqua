package aqua.model

import aqua.types.ArrowType
import cats.data.NonEmptyMap

// no service id
case class ServiceModel(
  name: String,
  arrows: NonEmptyMap[String, ArrowType],
  serviceId: Option[ValueModel]
) extends Model
