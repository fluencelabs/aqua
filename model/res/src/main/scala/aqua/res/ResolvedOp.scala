package aqua.res

import aqua.model.{CallModel, ForModel, ValueModel, VarModel}
import aqua.raw.ops.Call
import aqua.tree.{TreeNode, TreeNodeCompanion}
import cats.data.Chain
import cats.free.Cofree
import cats.Show

// TODO docs to all traits and objects
sealed trait ResolvedOp extends TreeNode[ResolvedOp]

object ResolvedOp extends TreeNodeCompanion[ResolvedOp] {

  given showTreeLabel: Show[ResolvedOp] =
    (op: ResolvedOp) => op.toString.stripSuffix("Res")
}

sealed trait NoAir extends ResolvedOp

case object SeqRes extends ResolvedOp
case object ParRes extends ResolvedOp
case object XorRes extends ResolvedOp

case class NextRes(item: String) extends ResolvedOp {
  override def toString: String = s"(next $item)"
}

case class MatchMismatchRes(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
    extends ResolvedOp {
  override def toString: String = s"(${if (shouldMatch) "match" else "mismatch"} $left $right)"
}

case class FoldRes(item: String, iterable: ValueModel, mode: Option[ForModel.Mode] = None) extends ResolvedOp {
  override def toString: String = s"(fold $iterable $item "
}

case class RestrictionRes(item: String, isStream: Boolean) extends ResolvedOp {
  override def toString: String = s"(new ${if (isStream) "$" else ""}$item "
}

case class CallServiceRes(
  serviceId: ValueModel,
  funcName: String,
  call: CallRes,
  peerId: ValueModel
) extends ResolvedOp {
  override def toString: String = s"(call $peerId ($serviceId $funcName) $call)"
}

case class ApRes(operand: ValueModel, exportTo: CallModel.Export) extends ResolvedOp {
  override def toString: String = s"(ap $operand $exportTo)"
}

case class CanonRes(operand: ValueModel, peerId: ValueModel, exportTo: CallModel.Export) extends ResolvedOp {
  override def toString: String = s"(canon $peerId $operand $exportTo)"
}

case object NullRes extends ResolvedOp {
  override def toString: String = "(null)"
}
