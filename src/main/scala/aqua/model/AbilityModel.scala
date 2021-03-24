package aqua.model

sealed trait AbilityModel extends Model

case class ServiceModel(name: String, id: ValueModel) extends AbilityModel
