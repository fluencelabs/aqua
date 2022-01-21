package aqua.model.transform.funcop

import aqua.model.transform.pre.InitPeerCallable
import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  MatchMismatchModel,
  OnModel,
  OpModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
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

  def transform(op: OpModel.Tree): OpModel.Tree =
    if (enabled) {
      var i = 0
      Cofree
        .cata[Chain, OpModel, OpModel.Tree](op) {
          case (ot @ (OnModel(_, _) | MatchMismatchModel(_, _, _)), children) =>
            i = i + 1
            Eval now ot.wrap(
              XorModel.wrap(
                SeqModel.wrap(children.toList: _*),
                callable.onInitPeer.wrap(
                  CallServiceModel(
                    serviceId,
                    funcName,
                    ErrorsCatcher.lastErrorCall(i)
                  ).leaf
                )
              )
            )

          case (tag, children) =>
            Eval.now(Cofree(tag, Eval.now(children)))
        }
        .value
    } else op

}

object ErrorsCatcher {

  val lastErrorArg: ValueModel =
    VarModel(ValueRaw.LastError.name, ValueRaw.LastError.baseType, Chain.empty)

  def lastErrorCall(i: Int): CallModel = CallModel(
    lastErrorArg :: LiteralModel.fromRaw(LiteralRaw.number(i)) :: Nil,
    Nil
  )
}
