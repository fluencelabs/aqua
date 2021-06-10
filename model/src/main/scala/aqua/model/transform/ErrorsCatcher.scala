package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps, MatchMismatchTag, OnTag, OpTag, XorTag}
import aqua.types.LiteralType
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
          case (tag, children)
              if children.length == 1 && children.headOption.exists(
                _.head == XorTag.LeftBiased
              ) =>
            Eval.now(Cofree(tag, Eval.now(children)))

          case (ot @ (OnTag(_, _) | MatchMismatchTag(_, _, _)), children) =>
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

  val lastErrorArg: ValueModel = VarModel.lastError

  def lastErrorCall(i: Int): Call = Call(
    lastErrorArg :: LiteralModel(i.toString, LiteralType.number) :: Nil,
    None
  )
}
