package aqua.generator

import aqua.model.FuncCallable

case class FuncAirGen(name: String, func: FuncCallable) {

  def bodyGen: AirGen = AirGen(func.body.tree)

  def generateAir: Air =
    bodyGen.generate

  def generateTs: TypescriptFunc =
    TypescriptFunc(name, func, generateTsAir)

  def generateTsAir: Air =
    AirGen(
      func.generateTsModel.tree
    ).generate
}
