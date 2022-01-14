package aqua.raw.ops

import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import cats.data.{Chain, NonEmptyList}

sealed trait RawTag {

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

case class XorParTag(xor: FuncOp, par: FuncOp) extends RawTag

case class OnTag(peerId: ValueRaw, via: Chain[ValueRaw]) extends SeqGroupTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    OnTag(peerId.map(f), via.map(_.map(f)))

  override def toString: String =
    s"(on $peerId${if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""})"
}

case class NextTag(item: String) extends RawTag {

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))
}

case class RestrictionTag(name: String, isStream: Boolean) extends SeqGroupTag {

  override def restrictsVarNames: Set[String] = Set(name)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(name = map.getOrElse(name, name))
}

case class MatchMismatchTag(left: ValueRaw, right: ValueRaw, shouldMatch: Boolean)
  extends SeqGroupTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    MatchMismatchTag(left.map(f), right.map(f), shouldMatch)
}

case class ForTag(item: String, iterable: ValueRaw) extends SeqGroupTag {

  override def restrictsVarNames: Set[String] = Set(item)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ForTag(item, iterable.map(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))
}

case class CallArrowTag(
                         funcName: String,
                         call: Call
                       ) extends RawTag {

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallArrowTag(funcName, call.mapValues(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(call = call.mapExport(n => map.getOrElse(n, n)))
}

case class DeclareStreamTag(
                             value: ValueRaw
                           ) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    DeclareStreamTag(value.map(f))
}

case class AssignmentTag(
                          value: ValueRaw,
                          assignTo: String
                        ) extends NoExecTag {

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    AssignmentTag(value.map(f), assignTo)
}

case class ClosureTag(
                       func: FuncRaw
                     ) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ClosureTag(
      func.copy(arrow =
        func.arrow.copy(
          ret = func.arrow.ret.map(_.map(f)),
          body = FuncOp(func.arrow.body.tree.map(_.mapValues(f)))
        )
      )
    )
}

case class ReturnTag(
                      values: NonEmptyList[ValueRaw]
                    ) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ReturnTag(values.map(_.map(f)))
}

object EmptyTag extends NoExecTag

case class AbilityIdTag(
                         value: ValueRaw,
                         service: String
                       ) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    AbilityIdTag(value.map(f), service)
}

case class CallServiceTag(
                           serviceId: ValueRaw,
                           funcName: String,
                           call: Call
                         ) extends RawTag {

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallServiceTag(serviceId.map(f), funcName, call.mapValues(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(call = call.mapExport(n => map.getOrElse(n, n)))

  override def toString: String = s"(call _ ($serviceId $funcName) $call)"
}

case class PushToStreamTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    PushToStreamTag(operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(push $operand $exportTo)"
}

case class CanonicalizeTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CanonicalizeTag(operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(can $operand $exportTo)"
}

case class JoinTag(operands: NonEmptyList[ValueRaw]) extends RawTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    JoinTag(operands.map(_.map(f)))

  override def toString: String = s"(join ${operands.toList.mkString(" ")})"
}
