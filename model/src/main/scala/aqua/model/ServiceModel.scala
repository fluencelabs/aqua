package aqua.model

import aqua.types.ArrowType
import cats.data.NonEmptyMap

case class ServiceModel(name: String, arrows: NonEmptyMap[String, ArrowType]) extends Model
