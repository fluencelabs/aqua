package aqua.model

import aqua.generator.DataView.InitPeerId
import aqua.generator.{Air, AirContext, AirGen, ArrowCallable, DataView, FuncCallable, SrvCallableOnPeer}
import aqua.semantics.{ArrowType, DataType}

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[DataView],
  body: FuncOp
) extends Model {

  def bodyGen: AirGen = body.toAirGen

  def callable: ArrowCallable =
    new FuncCallable(args.map(_._1), ret, bodyGen)

  def generateAir(acc: Map[String, ArrowCallable]): Air =
    bodyGen
      .generate(
        AirContext(
          data = args.collect { //TODO preload these variables
            case (an, Left(_)) =>
              an -> DataView.Variable(an)
          }.toMap,
          arrows = acc ++ args.collect { case (an, Right(_)) =>
            an -> new SrvCallableOnPeer(InitPeerId, DataView.StringScalar("callback"), an)
          }.toMap,
          vars = args.map(_._1).toSet
        )
      )
      ._2

}
