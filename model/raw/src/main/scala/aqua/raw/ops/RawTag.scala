package aqua.raw.ops

import aqua.errors.Errors.internalError
import aqua.raw.arrow.FuncRaw
import aqua.raw.value.{CallArrowRaw, CallServiceRaw, ValueRaw, VarRaw}
import aqua.tree.{TreeNode, TreeNodeCompanion}
import aqua.types.*

import cats.Show
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.syntax.foldable.*

sealed trait RawTag extends TreeNode[RawTag] {

  // What var names are exported – can be used AFTER this tag is executed
  def exportsVarNames: Set[String] = Set.empty

  // What var names are restricted only for children of this tag – CANNOT be used after this tag, only within
  def restrictsVarNames: Set[String] = Set.empty

  // All variable names introduced by this tag
  final def definesVarNames: Set[String] = exportsVarNames ++ restrictsVarNames

  // Variable names used by this tag (not introduced by it)
  def usesVarNames: Set[String] = Set.empty

  def mapValues(f: ValueRaw => ValueRaw): RawTag

  def renameExports(map: Map[String, String]): RawTag = this

  def funcOpLeaf: FuncOp = FuncOp(leaf)
}

object RawTag extends TreeNodeCompanion[RawTag] with RawTagGivens {

  given showTreeLabel: Show[RawTag] =
    (t: RawTag) => t.toString.stripSuffix("Tag")

  val empty: Tree = EmptyTag.leaf
}

sealed trait NoExecTag extends RawTag

sealed trait GroupTag extends RawTag {
  override def mapValues(f: ValueRaw => ValueRaw): RawTag = this
}

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

  override def wrap(children: Chain[Tree]): Tree =
    super.wrapNonEmpty(children.filterNot(_.head == EmptyTag), RawTag.empty)
}

case object ParTag extends ParGroupTag {

  /**
   * Used for `co` instruction
   */
  case object Detach extends GroupTag

  /**
   * This tag should be eliminated in semantics
   * and merged with [[ParTag]]
   *
   * Used for `par` instruction
   */
  case object Par extends GroupTag
}

case class IfTag(value: ValueRaw) extends GroupTag {

  override def usesVarNames: Set[String] = value.varNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    IfTag(value.map(f))
}

object IfTag {

  /**
   * This tag should be eliminated in semantics
   * and merged with [[IfTag]]
   */
  case object Else extends GroupTag
}

case object TryTag extends GroupTag {

  /**
   * This tag should be eliminated in semantics
   * and merged with [[TryTag]]
   */
  case object Catch extends GroupTag

  /**
   * This tag should be eliminated in semantics
   * and merged with [[TryTag]]
   */
  case object Otherwise extends GroupTag
}

case class OnTag(
  peerId: ValueRaw,
  via: Chain[ValueRaw],
  // Strategy of returning from this `on` block
  // affects handling of this `on` in topology layer
  strategy: Option[OnTag.ReturnStrategy] = None
) extends SeqGroupTag {

  override def usesVarNames: Set[String] = peerId.varNames ++ via.foldMap(_.varNames)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    OnTag(peerId.map(f), via.map(_.map(f)), strategy)

  override def toString: String = {
    val viaPart = if (via.nonEmpty) " via " + via.toList.mkString(" via ") else ""
    val strategyPart = strategy.fold("")(s => s" | $s")
    s"(on $peerId$viaPart$strategyPart)"
  }
}

object OnTag {

  // Return strategy of `on` block
  // affects handling of `on` in topology layer
  enum ReturnStrategy {
    // Leave peer to the first relay
    // Do not make the whole back transition
    // NOTE: used for `parseq`
    case Relay
  }
}

case class NextTag(item: String) extends RawTag {

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item))

  override def usesVarNames: Set[String] = Set(item)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag = this
}

case class ForKeyValue(key: String, value: String) {
  def toSet: Set[String] = Set(key, value)

  def rename(map: Map[String, String]): ForKeyValue =
    copy(key = map.getOrElse(key, key), value = map.getOrElse(value, value))
}

case class ForTag(
  item: String,
  iterable: ValueRaw,
  mode: ForTag.Mode,
  keyValue: Option[ForKeyValue] = None
) extends SeqGroupTag {

  override def restrictsVarNames: Set[String] = Set(item) ++ keyValue.toSet.flatMap(_.toSet)

  override def usesVarNames: Set[String] = iterable.varNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ForTag(item, iterable.map(f), mode, keyValue)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(item = map.getOrElse(item, item), keyValue = keyValue.map(_.rename(map)))
}

object ForTag {

  /**
   *  | Syntax      | mode | fold last | canon | inner tag | par null wrap |
   *  |-------------|:----:|:---------:|:-----:|:---------:|:-------------:|
   *  | for ...     | seq  |   null    |   +   |    seq    |       -       |
   *  | for ... par | par  |   never   |   +   |    par    |       +       |
   *  | for ... try | try  |   null    |   +   |    try    |       -       |
   *  | for ... rec | rec  |   never   |   -   |    par    |       +       |
   *  | parseq ...  | par  |   never   |   +   |    par    |       -       |
   */
  enum Mode {
    case ParMode, SeqMode, TryMode, RecMode
  }

  def par(item: String, iterable: ValueRaw, keyValue: Option[ForKeyValue] = None): ForTag =
    ForTag(item, iterable, Mode.ParMode, keyValue)

  def seq(item: String, iterable: ValueRaw, keyValue: Option[ForKeyValue] = None): ForTag =
    ForTag(item, iterable, Mode.SeqMode, keyValue)
}

case class CallArrowRawTag(
  exportTo: List[Call.Export],
  value: ValueRaw
) extends RawTag {

  private lazy val usesExportStreams = exportTo.collect { case Call.Export(name, _, true) =>
    name
  }.toSet

  // don't use existing streams in exports
  override def exportsVarNames: Set[String] = exportTo.map(_.name).toSet -- usesExportStreams

  override def usesVarNames: Set[String] = value.varNames ++ usesExportStreams

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CallArrowRawTag(exportTo.map(_.mapStream(f)), value.map(f))

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.map(_.renameNonStream(map)))
}

object CallArrowRawTag {

  def ability(
    abilityName: String,
    funcName: String,
    call: Call,
    arrowType: ArrowType
  ): CallArrowRawTag =
    CallArrowRawTag(
      call.exportTo,
      CallArrowRaw.ability(
        abilityName,
        funcName,
        arrowType,
        call.args
      )
    )

  def func(fnName: String, call: Call): CallArrowRawTag =
    CallArrowRawTag(
      call.exportTo,
      CallArrowRaw.func(
        funcName = fnName,
        baseType = call.arrowType,
        arguments = call.args
      )
    )

  def service(
    srvId: ValueRaw,
    funcName: String,
    call: Call,
    arrowType: Option[ArrowType] = None
  ): CallArrowRawTag =
    CallArrowRawTag(
      call.exportTo,
      CallServiceRaw(
        srvId,
        funcName,
        arrowType.getOrElse(call.arrowType),
        call.args
      )
    )
}

case class DeclareStreamTag(
  name: String,
  `type`: MutableStreamType
) extends RawTag {

  override def exportsVarNames: Set[String] = Set(name)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    f(VarRaw(name, `type`)) match {
      case VarRaw(name, t: MutableStreamType) => copy(name, t)
      case v =>
        internalError(s"DeclareStreamTag can be only VarRaw with stream type, currently: '$v' ")
    }
}

case class AssignmentTag(
  value: ValueRaw,
  assignTo: String
) extends NoExecTag {

  override def exportsVarNames: Set[String] = Set(assignTo)

  override def usesVarNames: Set[String] = value.varNames

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    AssignmentTag(value.map(f), assignTo)
}

case class ClosureTag(
  func: FuncRaw,
  detach: Boolean
) extends NoExecTag {

  override def exportsVarNames: Set[String] = Set(func.name)

  override def usesVarNames: Set[String] = func.capturedVars

  override def renameExports(map: Map[String, String]): RawTag =
    copy(func =
      func.copy(
        name = map.getOrElse(func.name, func.name),
        arrow = func.arrow.copy(
          body = func.arrow.body.map(_.renameExports(map))
        )
      )
    )

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

  override def usesVarNames: Set[String] = values.foldMap(_.varNames)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ReturnTag(values.map(_.map(f)))
}

object EmptyTag extends NoExecTag {
  override def mapValues(f: ValueRaw => ValueRaw): RawTag = this
}

/**
 * Tag for `Service "id"` expression.
 * For each such expression new ability
 * is created with unique name (@p name).
 *
 * @param value value of service ID
 * @param serviceType type of service
 * @param name **rename** of service
 */
case class ServiceIdTag(
  value: ValueRaw,
  serviceType: ServiceType,
  name: String
) extends NoExecTag {

  override def usesVarNames: Set[String] = value.varNames

  override def exportsVarNames: Set[String] = Set(name)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(name = map.getOrElse(name, name))

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    ServiceIdTag(value.map(f), serviceType, name)
}

case class PushToStreamTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  /**
   * NOTE: Pushing to a stream will create it, but we suppose
   * that `DeclareStreamTag` exports stream and this tag does not
   * to distinguish cases when stream is captured from outside.
   * This is why `exportTo` is not in `exportsVarNames`.
   */
  override def exportsVarNames: Set[String] = Set.empty

  override def usesVarNames: Set[String] = operand.varNames + exportTo.name

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    PushToStreamTag(operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(push $operand $exportTo)"
}

case class PushToMapTag(key: ValueRaw, operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  override def exportsVarNames: Set[String] = Set.empty

  override def usesVarNames: Set[String] = key.varNames ++ operand.varNames + exportTo.name

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    PushToMapTag(key.map(f), operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(pushmap ($key $operand) $exportTo)"
}

case class FlattenTag(operand: ValueRaw, assignTo: String) extends RawTag {

  override def exportsVarNames: Set[String] = Set(assignTo)

  override def usesVarNames: Set[String] = operand.varNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    FlattenTag(operand.map(f), assignTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(assignTo = map.getOrElse(assignTo, assignTo))

  override def toString: String = s"(flat $operand $assignTo)"
}

case class CanonicalizeTag(operand: ValueRaw, exportTo: Call.Export) extends RawTag {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def usesVarNames: Set[String] = operand.varNames

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    CanonicalizeTag(operand.map(f), exportTo)

  override def renameExports(map: Map[String, String]): RawTag =
    copy(exportTo = exportTo.mapName(n => map.getOrElse(n, n)))

  override def toString: String = s"(can $operand $exportTo)"
}

case class JoinTag(operands: NonEmptyList[ValueRaw]) extends RawTag {

  override def usesVarNames: Set[String] = operands.foldMap(_.varNames)

  override def mapValues(f: ValueRaw => ValueRaw): RawTag =
    JoinTag(operands.map(_.map(f)))

  override def toString: String = s"(join ${operands.toList.mkString(" ")})"
}
