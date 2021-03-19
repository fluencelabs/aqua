package aqua.model

import aqua.generator.DataView.InitPeerId
import aqua.generator.{AirContext, ArrowGen, DataView, FuncBodyGen, FuncGen}
import aqua.semantics.algebra.types.{ArrowType, DataType}
import cats.Eval

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[DataView],
  body: FuncBodyGen
) {

  def gen: FuncGen =
    FuncGen(
      name,
      Eval.later {
        body.op
          .generate(
            AirContext(
              data = args.collect { //TODO preload these variables
                case (an, Left(_)) =>
                  an -> DataView.Variable(an)
              }.toMap,
              arrows = args.collect { case (an, Right(_)) =>
                an -> new ArrowGen.SrvCallableOnPeer(InitPeerId, DataView.StringScalar("callback"), an)
              }.toMap,
              vars = args.map(_._1).toSet
            )
          )
          ._2
      },
      body
    )

}
