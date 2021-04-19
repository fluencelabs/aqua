package aqua.model.func

import aqua.types.{ArrowType, DataType, Type}

sealed abstract class ArgDef(val `type`: Type) {
  def name: String
}

object ArgDef {
  case class Data(name: String, dataType: DataType) extends ArgDef(dataType)
  case class Arrow(name: String, arrowType: ArrowType) extends ArgDef(arrowType)
}
