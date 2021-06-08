package aqua.model

case class ConstantModel(name: String, value: ValueModel, allowOverrides: Boolean) extends Model
