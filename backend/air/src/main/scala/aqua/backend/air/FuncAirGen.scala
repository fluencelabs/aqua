package aqua.backend.air

import aqua.model.FuncCallable

case class FuncAirGen(name: String, func: FuncCallable) {

  def bodyGen: AirGen = AirGen(func.body.tree)

  def generateAir: Air =
    bodyGen.generate

  def generateTsAir: Air =
    AirGen(
      func.generateTsModel.tree
    ).generate
}
