package aqua.generator

import aqua.model.FuncModel

case class FuncAirGen(func: FuncModel) {

  def bodyGen: AirGen = AirGen(func.body)

  def callable: ArrowCallable =
    new FuncCallable(func.args, func.ret.map(_._1).map(AirGen.valueToData), bodyGen)

  def airContextWithArgs(acc: Map[String, ArrowCallable]): AirContext =
    AirContext(
      data = func.args.collect { case (an, Left(_)) =>
        an -> DataView.Variable(an)
      }.toMap,
      arrows = acc ++ func.args.collect { case (an, Right(_)) =>
        an -> new SrvCallableOnPeer(DataView.InitPeerId, DataView.StringScalar(func.callbackService), an)
      }.toMap,
      vars = func.args.map(_._1).toSet
    )

  def generateAir(acc: Map[String, ArrowCallable]): Air =
    bodyGen
      .generate(airContextWithArgs(acc))
      ._2

  def generateTs(acc: Map[String, ArrowCallable]): TypescriptFunc =
    TypescriptFunc(func, generateTsAir(acc))

  def generateTsAir(acc: Map[String, ArrowCallable]): Air =
    AirGen(
      func.generateTsModel
    ).generate {
      val ctx = airContextWithArgs(acc)

      ctx.copy(data = Map.empty, vars = ctx.arrows.keySet)
    }._2
}
