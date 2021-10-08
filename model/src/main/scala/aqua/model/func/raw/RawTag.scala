package aqua.model.func.raw

import aqua.model.ValueModel
import aqua.model.func.{Call, FuncModel}
import cats.data.Chain

sealed trait RawTag {

  def mapValues(f: ValueModel => ValueModel): RawTag = this match {
    case OnTag(peerId, via) => OnTag(f(peerId), via.map(f))
    case MatchMismatchTag(left, right, shouldMatch) =>
      MatchMismatchTag(f(left), f(right), shouldMatch)
    case ForTag(item, iterable) => ForTag(item, f(iterable))
    case CallArrowTag(funcName, call) =>
      CallArrowTag(
        funcName,
        call.mapValues(f)
      )
    case CallServiceTag(serviceId, funcName, call) =>
      CallServiceTag(
        f(serviceId),
        funcName,
        call.mapValues(f)
      )
    case ApTag(operand, exportTo) =>
      ApTag(
        f(operand),
        exportTo
      )
    case AssignmentTag(value, assignTo) =>
      AssignmentTag(f(value), assignTo)
    case AbilityIdTag(value, ability) =>
      AbilityIdTag(f(value), ability)
    case _ => this
  }

}

sealed trait NoExecTag extends RawTag

sealed trait GroupTag extends RawTag
sealed trait SeqGroupTag extends GroupTag
sealed trait ParGroupTag extends GroupTag

case object SeqTag extends SeqGroupTag

case object ParTag extends ParGroupTag {
  case object Detach extends ParGroupTag
}

case object XorTag extends SeqGroupTag {
  case object LeftBiased extends SeqGroupTag
}
case class XorParTag(xor: FuncOp, par: FuncOp) extends RawTag

case class OnTag(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupTag {

  override def toString: String =
    s"(on $peerId${if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""})"
}
case class NextTag(item: String) extends RawTag

case class MatchMismatchTag(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
    extends SeqGroupTag
case class ForTag(item: String, iterable: ValueModel) extends SeqGroupTag

case class CallArrowTag(
  funcName: String,
  call: Call
) extends RawTag

case class AssignmentTag(
  value: ValueModel,
  assignTo: String
) extends NoExecTag

case class ClosureTag(
  func: FuncModel
) extends NoExecTag

object EmptyTag extends NoExecTag

case class AbilityIdTag(
  value: ValueModel,
  service: String
) extends NoExecTag

case class CallServiceTag(
  serviceId: ValueModel,
  funcName: String,
  call: Call
) extends RawTag {
  override def toString: String = s"(call _ ($serviceId $funcName) $call)"
}

case class ApTag(operand: ValueModel, exportTo: Call.Export) extends RawTag {
  override def toString: String = s"(ap $operand $exportTo)"
}
