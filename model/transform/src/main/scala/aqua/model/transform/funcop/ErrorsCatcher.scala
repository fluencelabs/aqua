package aqua.model.transform.funcop

import aqua.raw.ops.{Call, FuncOp, FuncOps, MatchMismatchTag, OnTag, RawTag, XorTag}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.LiteralType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class ErrorsCatcher(
  enabled: Boolean,
  serviceId: ValueRaw,
  funcName: String,
  callable: InitPeerCallable
) {

  def transform(op: FuncOp): FuncOp =
    if (enabled) {
      var i = 0
      op
        .cata[Cofree[Chain, RawTag]] {
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

  val lastErrorArg: ValueRaw = ValueRaw.LastError

  def lastErrorCall(i: Int): Call = Call(
    lastErrorArg :: LiteralRaw.number(i) :: Nil,
    Nil
  )
}
