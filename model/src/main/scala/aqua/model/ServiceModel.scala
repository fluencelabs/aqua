package aqua.model

import aqua.types.{ArrowType, ProductType}
import cats.data.NonEmptyMap

case class ServiceModel(
  name: String,
  arrows: NonEmptyMap[String, ArrowType],
  defaultId: Option[ValueModel]
) extends Model {
  def `type`: ProductType = ProductType(name, arrows)
}
