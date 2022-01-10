package aqua.raw.ops

import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import cats.data.{Chain, NonEmptyList}

sealed trait RawTag {
  // What variable names this tag uses (children are not respected)
  def usesVarNames: Set[String] = Set.empty

  def mapValues(f: ValueRaw => ValueRaw): RawTag = this match {
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
    case PushToStreamTag(operand, exportTo) =>
      PushToStreamTag(
        f(operand),
        exportTo
      )
    case CanonicalizeTag(operand, exportTo) =>
      CanonicalizeTag(
        f(operand),
        exportTo
      )
    case AssignmentTag(value, assignTo) =>
      AssignmentTag(f(value), assignTo)
    case ReturnTag(values) =>
      ReturnTag(values.map(f))
    case DeclareStreamTag(value) =>
      DeclareStreamTag(f(value))
    case AbilityIdTag(value, ability) =>
      AbilityIdTag(f(value), ability)
    case ClosureTag(func) =>
      ClosureTag(
        func.copy(arrow =
          func.arrow.copy(
            ret = func.arrow.ret.map(f),
            body = FuncOp(func.arrow.body.tree.map(_.mapValues(f)))
          )
        )
      )
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

case object XorTag extends GroupTag {
  case object LeftBiased extends GroupTag
}

case class XorParTag(xor: FuncOp, par: FuncOp) extends RawTag {
  // Collect all the used variable names
  override def usesVarNames: Set[String] = xor.usesVarNames.value ++ par.usesVarNames.value
}

case class OnTag(peerId: ValueRaw, via: Chain[ValueRaw]) extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    peerId.usesVarNames ++ via.iterator.flatMap(_.usesVarNames)

  override def toString: String =
    s"(on $peerId${if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""})"
}

case class NextTag(item: String) extends RawTag {
  override def usesVarNames: Set[String] = Set(item)
}

case class RestrictionTag(name: String, isStream: Boolean) extends SeqGroupTag {
  override def usesVarNames: Set[String] = Set(name)
}

case class MatchMismatchTag(left: ValueRaw, right: ValueRaw, shouldMatch: Boolean)
    extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    left.usesVarNames ++ right.usesVarNames
}

case class ForTag(item: String, iterable: ValueRaw) extends SeqGroupTag {
  override def usesVarNames: Set[String] = Set(item) ++ iterable.usesVarNames
}

case class CallArrowTag(
  funcName: String,
  call: Call
) extends RawTag {
  override def usesVarNames: Set[String] = call.argVarNames
}

case class DeclareStreamTag(
  value: ValueRaw
) extends NoExecTag {
  override def usesVarNames: Set[String] = value.usesVarNames
}

case class AssignmentTag(
  value: ValueRaw,
  assignTo: String
) extends NoExecTag {
  override def usesVarNames: Set[String] = Set(assignTo) ++ value.usesVarNames
}

case class ClosureTag(
  func: FuncRaw
) extends NoExecTag {
  // TODO captured names are lost?
  override def usesVarNames: Set[String] = Set(func.name)
}

case class ReturnTag(
  values: NonEmptyList[ValueRaw]
) extends NoExecTag

object EmptyTag extends NoExecTag

case class AbilityIdTag(
  value: ValueRaw,
  service: String
) extends NoExecTag

case class CallServiceTag(
  serviceId: ValueRaw,
  funcName: String,
  call: Call
) extends RawTag {

  override def usesVarNames: Set[String] = serviceId.usesVarNames ++ call.argVarNames

  override def toString: String = s"(call _ ($serviceId $funcName) $call)"
}

case class PushToStreamTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = operand.usesVarNames

  override def toString: String = s"(push $operand $exportTo)"
}

case class CanonicalizeTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = operand.usesVarNames

  override def toString: String = s"(can $operand $exportTo)"
}
