package aqua.model

import cats.data.Chain
import cats.free.Cofree
import cats.Eval
import cats.data.NonEmptyList

sealed trait OpModel {
  def leaf: OpModel.Tree = Cofree(this, Eval.now(Chain.empty))
}

object OpModel {
  type Tree = Cofree[Chain, OpModel]
  private val nil: Eval[Chain[Tree]] = Eval.now(Chain.empty)

  private def wrapIfNE(op: OpModel, children: List[Tree]): Tree =
    children match {
      case Nil => op.leaf
      case x :: Nil => x
      case ch => Cofree(op, Eval.now(Chain.fromSeq(ch)))
    }

  def par(children: Seq[Tree]): Tree =
    wrapIfNE(ParModel, children.toList)

  def seq(children: Seq[Tree]): Tree =
    wrapIfNE(SeqModel, children.toList)

  def pushToStream(what: ValueModel, stream: String): Tree =
    Cofree(PushToStreamModel(what, stream), nil)
}

sealed trait NoExecModel extends OpModel

sealed trait GroupOpModel extends OpModel

sealed trait SeqGroupModel extends GroupOpModel

case object SeqModel extends SeqGroupModel

case object ParModel extends GroupOpModel

case object XorModel extends GroupOpModel

case class OnModel(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupModel

case class NextModel(item: String) extends OpModel

case class RestrictionModel(name: String, isStream: Boolean) extends SeqGroupModel

case class MatchMismatchModel(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
    extends SeqGroupModel

case class ForModel(item: String, iterable: ValueModel) extends SeqGroupModel

case class DeclareStreamModel(value: ValueModel) extends NoExecModel

case class FlattenModel(value: ValueModel, assignTo: String) extends OpModel

case class PushToStreamModel(value: ValueModel, assignTo: String) extends OpModel

case class CallServiceModel(serviceId: ValueModel, funcName: String, call: CallModel)
    extends OpModel

case class CanonicalizeModel(operand: ValueModel, exportTo: CallModel.Export) extends OpModel

case class JoinModel(operands: NonEmptyList[ValueModel]) extends OpModel
