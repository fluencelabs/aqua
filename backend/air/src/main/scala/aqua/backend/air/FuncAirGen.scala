package aqua.backend.air

import aqua.model.FuncResolved

case class FuncAirGen(func: FuncResolved) {

  def bodyGen: AirGen = AirGen(func.func.body.tree)

  def generateAir: Air =
    bodyGen.generate

  def generateTsAir: Air =
    AirGen(
      func.generateTsModel.tree
    ).generate
}
