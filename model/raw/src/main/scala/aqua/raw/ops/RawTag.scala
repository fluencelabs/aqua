package aqua.raw.ops

import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import cats.data.{Chain, NonEmptyList}

sealed trait RawTag {
  // What variable names this tag uses (children are not respected)
  def usesVarNames: Set[String] = Set.empty

  // What var names are exported – can be used AFTER this tag is executed
  def exportsVarNames: Set[String] = Set.empty

  // What var names are restricted only for children of this tag – CANNOT be used after this tag, only within
  def restrictsVarNames: Set[String] = Set.empty

  // All variable names introduced by this tag
  def definesVarNames: Set[String] = exportsVarNames ++ restrictsVarNames

  def mapValues(f: ValueRaw => ValueRaw): RawTag = this

  def renameExports(map: Map[String, String]): RawTag = this

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

  override def exportsVarNames: Set[String] = xor.usesVarNames.value ++ par.usesVarNames.value
}

case class OnTag(peerId: ValueRaw, via: Chain[ValueRaw]) extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    peerId.usesVarNames ++ via.iterator.flatMap(_.usesVarNames)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    OnTag(f(peerId), via.map(f))

  override def toString: String =
    s"(on $peerId${if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""})"
}

case class NextTag(item: String) extends RawTag {
  override def usesVarNames: Set[String] = Set(item)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))
}

case class RestrictionTag(name: String, isStream: Boolean) extends SeqGroupTag {
  override def usesVarNames: Set[String] = Set.empty

  override def restrictsVarNames: Set[String] = Set(name)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(name = map.getOrElse(name, name))
}

case class MatchMismatchTag(left: ValueRaw, right: ValueRaw, shouldMatch: Boolean)
    extends SeqGroupTag {

  override def usesVarNames: Set[String] =
    left.usesVarNames ++ right.usesVarNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    MatchMismatchTag(f(left), f(right), shouldMatch)
}

case class ForTag(item: String, iterable: ValueRaw) extends SeqGroupTag {
  override def usesVarNames: Set[String] = iterable.usesVarNames

  override def restrictsVarNames: Set[String] = Set(item)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ForTag(item, f(iterable))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))
}

case class CallArrowTag(
  funcName: String,
  call: Call
) extends RawTag {
  override def usesVarNames: Set[String] = call.argVarNames

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallArrowTag(funcName, call.mapValues(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(call = call.mapExport(n => map.getOrElse(n, n)))
}

case class DeclareStreamTag(
  value: ValueRaw
) extends NoExecTag {
  override def usesVarNames: Set[String] = value.usesVarNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    DeclareStreamTag(f(value))
}

case class AssignmentTag(
  value: ValueRaw,
  assignTo: String
) extends NoExecTag {
  override def usesVarNames: Set[String] = Set(assignTo) ++ value.usesVarNames

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    AssignmentTag(f(value), assignTo)
}

case class ClosureTag(
  func: FuncRaw
) extends NoExecTag {
  // TODO captured names are lost?
  override def usesVarNames: Set[String] = Set(func.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ClosureTag(
      func.copy(arrow =
        func.arrow.copy(
          ret = func.arrow.ret.map(f),
          body = FuncOp(func.arrow.body.tree.map(_.mapValues(f)))
        )
      )
    )
}

case class ReturnTag(
  values: NonEmptyList[ValueRaw]
) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ReturnTag(values.map(f))
}

object EmptyTag extends NoExecTag

case class AbilityIdTag(
  value: ValueRaw,
  service: String
) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    AbilityIdTag(f(value), service)
}

case class CallServiceTag(
  serviceId: ValueRaw,
  funcName: String,
  call: Call
) extends RawTag {

  override def usesVarNames: Set[String] = serviceId.usesVarNames ++ call.argVarNames

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallServiceTag(f(serviceId), funcName, call.mapValues(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(call = call.mapExport(n => map.getOrElse(n, n)))

  override def toString: String = s"(call _ ($serviceId $funcName) $call)"
}

case class PushToStreamTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = operand.usesVarNames

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    PushToStreamTag(f(operand), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(push $operand $exportTo)"
}

case class CanonicalizeTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {
  override def usesVarNames: Set[String] = operand.usesVarNames

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CanonicalizeTag(f(operand), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(can $operand $exportTo)"
}

case class FlattenTag(operand: ValueRaw, assignTo: String) extends RawTag {
  override def usesVarNames: Set[String] = operand.usesVarNames

  override def exportsVarNames: Set[String] = Set(assignTo)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    FlattenTag(f(operand), assignTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def toString: String = s"(ap $operand $assignTo)"
}
