package aqua.model

import aqua.generator.DataView.{InitPeerId, StringScalar}
import aqua.generator.{
  Air,
  AirContext,
  AirGen,
  ArrowCallable,
  DataView,
  FuncCallable,
  SrvCallableOnPeer,
  TypescriptFunc
}
import aqua.semantics.{ArrowType, DataType, Type}
import cats.data.{Chain, NonEmptyChain}

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(DataView, Type)],
  body: FuncOp
) extends Model {

  def bodyGen: AirGen = body.toAirGen

  def getDataService: String = "getDataSrv"
  def callbackService: String = "callbackSrv"

  def callable: ArrowCallable =
    new FuncCallable(args, ret.map(_._1), bodyGen)

  def airContextWithArgs(acc: Map[String, ArrowCallable]): AirContext =
    AirContext(
      data = args.collect { case (an, Left(_)) =>
        an -> DataView.Variable(an)
      }.toMap,
      arrows = acc ++ args.collect { case (an, Right(_)) =>
        an -> new SrvCallableOnPeer(InitPeerId, DataView.StringScalar(callbackService), an)
      }.toMap,
      vars = args.map(_._1).toSet
    )

  def generateAir(acc: Map[String, ArrowCallable]): Air =
    bodyGen
      .generate(airContextWithArgs(acc))
      ._2

  def generateTs(acc: Map[String, ArrowCallable]): TypescriptFunc =
    TypescriptFunc(this, generateTsAir(acc))

  val respFuncName = "response"

  val returnCallback: Option[FuncOp] = ret.map { case (dv, t) =>
    viaRelay(
      CoalgebraModel(
        Some(ServiceModel(callbackService, StringScalar("\"" + callbackService + "\""))),
        respFuncName,
        (dv, t) :: Nil,
        None
      )
    )

  }

  def generateTsAir(acc: Map[String, ArrowCallable]): Air =
    SeqModel(
      NonEmptyChain
        .fromChainAppend(
          Chain.fromSeq(
            args.collect { case (argName, Left(_)) =>
              getDataOp(argName)
            } :+ getDataOp("relay")
          ),
          body
        )
        .appendChain(Chain.fromSeq(returnCallback.toSeq))
    ).toAirGen.generate {
      val ctx = airContextWithArgs(acc)

      ctx.copy(data = Map.empty, vars = ctx.arrows.keySet)
    }._2

  def getDataOp(name: String): FuncOp =
    CoalgebraModel(
      Some(ServiceModel(getDataService, StringScalar("\"" + getDataService + "\""))),
      name,
      Nil,
      Some(name)
    )

  def viaRelay(op: FuncOp): FuncOp =
    OnModel(
      DataView.Variable("relay"),
      SeqModel(
        NonEmptyChain(
          CoalgebraModel(
            Some(ServiceModel("op", StringScalar("\"op\""))),
            "identity",
            Nil,
            None
          ),
          OnModel(InitPeerId, op)
        )
      )
    )

}
