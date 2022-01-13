package aqua.model.transform.res

import aqua.model.{CallModel, ValueModel, VarModel}
import aqua.raw.ops.Call

// TODO docs to all traits and objects
sealed trait ResolvedOp

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

case class FoldRes(item: String, iterable: ValueModel) extends ResolvedOp {
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

case class ApRes(operand: ValueModel, exportTo: Call.Export) extends ResolvedOp {
  override def toString: String = s"(ap $operand $exportTo)"

  def mapValues(f: ValueModel => ValueModel): ApRes =
    ApRes(f(operand), exportTo)

  def mapExport(f: String => String): ApRes = copy(exportTo = exportTo.mapName(f))
}
