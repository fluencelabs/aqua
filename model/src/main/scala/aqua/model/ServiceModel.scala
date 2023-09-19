package aqua.model

import aqua.types.ServiceType

case class ServiceModel(
  name: String,
  `type`: ServiceType,
  defaultId: Option[ValueModel]
)
