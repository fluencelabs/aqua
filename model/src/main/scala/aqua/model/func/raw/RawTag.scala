package aqua.model.func.raw

import aqua.model.ValueModel
import aqua.model.ValueModel.varName
import aqua.model.func.{Call, FuncModel}
import cats.data.NonEmptyList
import cats.data.Chain

sealed trait RawTag {
  def usesVarNames: Set[String] = Set.empty

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
  override def usesVarNames: Set[String] = xor.head.usesVarNames ++ par.head.usesVarNames
}

case class OnTag(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    ValueModel.varName(peerId).toSet ++ via.iterator.flatMap(ValueModel.varName)

  override def toString: String =
    s"(on $peerId${if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""})"
}

case class NextTag(item: String) extends RawTag {
  override def usesVarNames: Set[String] = Set(item)
}

case class RestrictionTag(name: String, isStream: Boolean) extends SeqGroupTag {
  override def usesVarNames: Set[String] = Set(name)
}

case class MatchMismatchTag(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
  extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    ValueModel.varName(left).toSet ++ ValueModel.varName(right).toSet
}

case class ForTag(item: String, iterable: ValueModel) extends SeqGroupTag {
  override def usesVarNames: Set[String] = Set(item) ++ ValueModel.varName(iterable)
}

case class CallArrowTag(
                         funcName: String,
                         call: Call
                       ) extends RawTag {
  override def usesVarNames: Set[String] = call.argVarNames
}

case class DeclareStreamTag(
                             value: ValueModel
                           ) extends NoExecTag {
  override def usesVarNames: Set[String] = ValueModel.varName(value).toSet
}

case class AssignmentTag(
                          value: ValueModel,
                          assignTo: String
                        ) extends NoExecTag {
  override def usesVarNames: Set[String] = Set(assignTo) ++ ValueModel.varName(value)
}

case class ClosureTag(
                       func: FuncModel
                     ) extends NoExecTag {
  // TODO captured names are lost?
  override def usesVarNames: Set[String] = Set(func.name)
}

case class ReturnTag(
                      values: NonEmptyList[ValueModel]
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

  override def usesVarNames: Set[String] = ValueModel.varName(serviceId).toSet ++ call.argVarNames

  override def toString: String = s"(call _ ($serviceId $funcName) $call)"
}

case class PushToStreamTag(operand: ValueModel, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = ValueModel.varName(operand).toSet

  override def toString: String = s"(push $operand $exportTo)"
}

case class CanonicalizeTag(operand: ValueModel, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = ValueModel.varName(operand).toSet

  override def toString: String = s"(can $operand $exportTo)"
}
