package aqua.model

import aqua.generator.DataView

sealed trait AbilityModel

case class ServiceModel(name: String, id: DataView) extends AbilityModel
