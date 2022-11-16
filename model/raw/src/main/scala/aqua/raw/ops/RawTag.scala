package aqua.raw.ops

import aqua.raw.Raw
import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.RawTag.Tree
import aqua.raw.value.{CallArrowRaw, ValueRaw}
import aqua.tree.{TreeNode, TreeNodeCompanion}
import aqua.types.{ArrowType, ProductType}
import cats.{Eval, Show}
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree

sealed trait RawTag extends TreeNode[RawTag] {

  // What var names are exported – can be used AFTER this tag is executed
  def exportsVarNames: Set[String] = Set.empty

  // What var names are restricted only for children of this tag – CANNOT be used after this tag, only within
  def restrictsVarNames: Set[String] = Set.empty

  // All variable names introduced by this tag
  def definesVarNames: Set[String] = exportsVarNames ++ restrictsVarNames

  def mapValues(f: ValueRaw => ValueRaw): RawTag = this

  def renameExports(map: Map[String, String]): RawTag = this

  def funcOpLeaf: FuncOp = FuncOp(leaf)
}

object RawTag extends TreeNodeCompanion[RawTag] with RawTagGivens {

  given showTreeLabel: Show[RawTag] =
    (t: RawTag) => t.toString.stripSuffix("Tag")

  val empty: Tree = EmptyTag.leaf
}

sealed trait NoExecTag extends RawTag

sealed trait GroupTag extends RawTag

sealed trait SeqGroupTag extends GroupTag

object SeqGroupTag extends SeqGroupTag {
  override def toString: String = "SeqGroup"

  def ungroupSingle(tree: Tree): Tree = tree.head match {
    case SeqGroupTag =>
      val children = tree.tail.value
      children.headOption.fold(tree) {
        case h if children.length == 1 => h
        case _ => tree
      }
    case _ => tree
  }
}

sealed trait ParGroupTag extends GroupTag

case object SeqTag extends SeqGroupTag {

  override def wrap(children: Tree*): Tree =
    super.wrapNonEmpty(children.filterNot(_.head == EmptyTag).toList, RawTag.empty)
}

case object ParTag extends ParGroupTag {
  case object Detach extends ParGroupTag
}

case object XorTag extends GroupTag {
  case object LeftBiased extends GroupTag
}

case class XorParTag(xor: RawTag.Tree, par: RawTag.Tree) extends RawTag

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

case class ForTag(item: String, iterable: ValueRaw, mode: Option[ForTag.Mode] = None) extends SeqGroupTag {

  override def restrictsVarNames: Set[String] = Set(item)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ForTag(item, iterable.map(f), mode)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))
}

object ForTag {
  sealed trait Mode
  case object WaitMode extends Mode
  case object PassMode extends Mode
}

case class CallArrowRawTag(
  exportTo: List[Call.Export],
  value: ValueRaw
) extends RawTag {

  override def exportsVarNames: Set[String] = exportTo.map(_.name).toSet

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallArrowRawTag(exportTo, value.map(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.map(_.mapName(n => map.getOrElse(n, n))))
}

object CallArrowRawTag {

  def service(
    serviceId: ValueRaw,
    fnName: String,
    call: Call,
    name: String = null,
    arrowType: ArrowType = null
  ): CallArrowRawTag =
    CallArrowRawTag(
      call.exportTo,
      CallArrowRaw(
        Option(name),
        fnName,
        call.args,
        Option(arrowType).getOrElse(
          call.arrowType
        ),
        Some(serviceId)
      )
    )

  def func(fnName: String, call: Call): CallArrowRawTag =
    CallArrowRawTag(
      call.exportTo,
      CallArrowRaw(
        None,
        fnName,
        call.args,
        call.arrowType,
        None
      )
    )
}

case class DeclareStreamTag(
  value: ValueRaw
) extends RawTag {

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
  func: FuncRaw,
  detach: Boolean
) extends NoExecTag {

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    copy(
      func.copy(arrow =
        func.arrow.copy(
          ret = func.arrow.ret.map(_.map(f)),
          body = func.arrow.body.map(_.mapValues(f))
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

case class PushToStreamTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    PushToStreamTag(operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(push $operand $exportTo)"
}

case class FlattenTag(operand: ValueRaw, assignTo: String) extends RawTag {

  override def exportsVarNames: Set[String] = Set(assignTo)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    FlattenTag(operand.map(f), assignTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def toString: String = s"(flat $operand $assignTo)"
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
