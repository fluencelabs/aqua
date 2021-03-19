package aqua.model

import aqua.generator.DataView.InitPeerId
import aqua.generator.{AirContext, DataView, FuncBodyGen, FuncCallable, FuncGen, SrvCallableOnPeer}
import aqua.semantics.{ArrowType, DataType}
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
      acc =>
        Eval.later {
          body.op
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
        },
      body,
      new FuncCallable(
        args.map(_._1),
        ret,
        body
      )
    )

}
