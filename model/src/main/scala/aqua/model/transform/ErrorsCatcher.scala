package aqua.model.transform

import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps, OnTag, OpTag}
import aqua.model.{LiteralModel, ValueModel}
import aqua.types.ScalarType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class ErrorsCatcher(
  enabled: Boolean,
  serviceId: ValueModel,
  funcName: String,
  callable: InitPeerCallable
) {

  def transform(op: FuncOp): FuncOp =
    if (enabled) {
      var i = 0
      op
        .cata[Cofree[Chain, OpTag]] {
          case (ot: OnTag, children) =>
            i = i + 1
            Eval now
              FuncOp
                .wrap(
                  ot,
                  FuncOps.xor(
                    FuncOps.seq(children.map(FuncOp(_)).toList: _*),
                    callable.makeCall(
                      serviceId,
                      funcName,
                      ErrorsCatcher.lastErrorCall(i)
                    )
                  )
                )
                .tree

          case (tag, children) =>
            Eval.now(Cofree(tag, Eval.now(children)))
        }
        .map(FuncOp(_))
        .value
    } else op

}

object ErrorsCatcher {
  // TODO not a string
  val lastErrorArg: ValueModel = LiteralModel("%last_error%", ScalarType.string)

  def lastErrorCall(i: Int): Call = Call(
    lastErrorArg :: LiteralModel(i.toString, ScalarType.string) :: Nil,
    None
  )
}
