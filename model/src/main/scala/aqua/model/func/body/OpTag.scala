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
    case CallFunctionTag(funcName, call) =>
      CallFunctionTag(
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
    case _ => this
  }

}

case object SeqTag extends OpTag
case object ParTag extends OpTag
case object XorTag extends OpTag
case class OnTag(peerId: ValueModel, via: Chain[ValueModel]) extends OpTag
case class NextTag(item: String) extends OpTag
case class MatchMismatchTag(left: ValueModel, right: ValueModel, shouldMatch: Boolean) extends OpTag
case class ForTag(item: String, iterable: ValueModel) extends OpTag

case class CallFunctionTag(
  funcName: String,
  call: Call
) extends OpTag

case class CallServiceTag(
  serviceId: ValueModel,
  funcName: String,
  call: Call,
  peerId: Option[ValueModel] = None
) extends OpTag
