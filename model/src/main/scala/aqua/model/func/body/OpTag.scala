package aqua.model.func.body

import aqua.model.ValueModel
import aqua.model.func.Call
import cats.data.Chain

sealed trait OpTag {

  def mapValues(f: ValueModel => ValueModel): OpTag = this match {
    case OnTag(peerId, via) => OnTag(f(peerId), via.map(f))
    case MatchMismatchTag(left, right, shouldMatch) =>
      MatchMismatchTag(f(left), f(right), shouldMatch)
    case ForTag(item, iterable) => ForTag(item, f(iterable))
    case CallArrowTag(funcName, call) =>
      CallArrowTag(
        funcName,
        call.mapValues(f)
      )
    case CallServiceTag(serviceId, funcName, call, pid) =>
      CallServiceTag(
        f(serviceId),
        funcName,
        call.mapValues(f),
        pid.map(f)
      )
    case AssignmentTag(value, assignTo) =>
      AssignmentTag(f(value), assignTo)
    case _ => this
  }

}
sealed trait GroupTag extends OpTag
sealed trait SeqGroupTag extends GroupTag

case object SeqTag extends SeqGroupTag
case object ParTag extends GroupTag

case object XorTag extends GroupTag {
  case object LeftBiased extends GroupTag
}
case class XorParTag(xor: FuncOp, par: FuncOp) extends OpTag
case class OnTag(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupTag
case class NextTag(item: String) extends OpTag

case class MatchMismatchTag(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
    extends SeqGroupTag
case class ForTag(item: String, iterable: ValueModel) extends SeqGroupTag

case class MetaTag(
  skipTopology: Boolean,
  comment: Option[String],
  op: OpTag
) extends OpTag

case class CallArrowTag(
  funcName: String,
  call: Call
) extends OpTag

case class AssignmentTag(
  value: ValueModel,
  assignTo: String
) extends OpTag

case class CallServiceTag(
  serviceId: ValueModel,
  funcName: String,
  call: Call,
  peerId: Option[ValueModel] = None
) extends OpTag
