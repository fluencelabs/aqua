package aqua.model

import aqua.model.OpModel.Tree
import cats.data.Chain
import cats.free.Cofree
import cats.Show
import cats.Eval
import cats.data.NonEmptyList
import aqua.tree.{TreeNode, TreeNodeCompanion}
import aqua.types.ScalarType

import scala.annotation.tailrec

sealed trait OpModel extends TreeNode[OpModel] {

  // What var names are restricted only for children of this tag – CANNOT be used after this tag, only within
  def restrictsVarNames: Set[String] = Set.empty

  // What variable names this tag uses (children are not respected)
  def usesVarNames: Set[String] = Set.empty

  // What var names are exported – can be used AFTER this tag is executed
  def exportsVarNames: Set[String] = Set.empty

}

object OpModel extends TreeNodeCompanion[OpModel] {

  given showTreeLabel: Show[OpModel] =
    (t: OpModel) => t.toString.stripSuffix("Model")

  def exportsVarNames(tree: Tree): Eval[Set[String]] = Cofree.cata(tree) { case (op, acc) =>
    Eval.later(acc.foldLeft(op.exportsVarNames)(_ ++ _) -- op.restrictsVarNames)
  }

  // TODO: as it is used for checking of intersection, make it a lazy traverse with fail-fast
  def usesVarNames(tree: Tree): Eval[Set[String]] = Cofree.cata(tree) { case (op, acc) =>
    Eval.later(acc.foldLeft(op.usesVarNames)(_ ++ _) -- op.restrictsVarNames)
  }
}

/**
 * Meta information embedded in a tree
 */
enum MetaModel extends OpModel {

  /**
   * Wraps subtree that was produced after inlining arrow
   *
   * @param name Name of arrow inlined
   */
  case CallArrowModel(name: String)

  override def wrap(children: Tree*): Tree =
    // NOTE: Consider leaving some meta info if call is completely erased?
    children.filter(_.head != EmptyModel) match {
      case Nil => EmptyModel.leaf
      case filtered => super.wrap(filtered: _*)
    }
}

sealed trait NoExecModel extends OpModel

sealed trait ForceExecModel extends OpModel

sealed trait GroupOpModel extends OpModel

sealed trait SeqGroupModel extends GroupOpModel

sealed trait ParGroupModel extends GroupOpModel

case object SeqModel extends SeqGroupModel {

  override def wrap(children: Tree*): Tree =
    super.wrapNonEmpty(children.filterNot(_.head == EmptyModel).toList, EmptyModel.leaf)

  // EmptyModel allowed – useful for tests
  def wrapWithEmpty(children: Tree*): Tree =
    super.wrapNonEmpty(children.toList, EmptyModel.leaf)

}

case object ParModel extends ParGroupModel

case object DetachModel extends ParGroupModel

case object XorModel extends GroupOpModel

case class OnModel(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupModel {

  override def toString: String =
    s"on $peerId${if (via.nonEmpty) s" via ${via.toList.mkString(", ")}" else ""}"

  override lazy val usesVarNames: Set[String] =
    peerId.usesVarNames ++ via.iterator.flatMap(_.usesVarNames)
}

case class NextModel(item: String) extends OpModel {
  override def usesVarNames: Set[String] = Set(item)

}

case class RestrictionModel(name: String, isStream: Boolean) extends SeqGroupModel {
  override def usesVarNames: Set[String] = Set.empty

  override def restrictsVarNames: Set[String] = Set(name)
}

case class MatchMismatchModel(left: ValueModel, right: ValueModel, shouldMatch: Boolean)
    extends SeqGroupModel {

  override def toString: String = s"if $left ${if (shouldMatch) "==" else "!="} $right"

  override def usesVarNames: Set[String] =
    left.usesVarNames ++ right.usesVarNames
}

case class ForModel(
  item: String,
  iterable: ValueModel,
  mode: Option[ForModel.Mode] = Some(ForModel.NullMode)
) extends SeqGroupModel {

  override def toString: String =
    s"for $item <- $iterable${mode.map(m => " " + m.toString).getOrElse("")}"

  override def restrictsVarNames: Set[String] = Set(item)

  override def usesVarNames: Set[String] = iterable.usesVarNames

}

object ForModel {
  sealed trait Mode
  case object NullMode extends Mode
  case object NeverMode extends Mode
}

// TODO how is it used? remove, if it's not
case class DeclareStreamModel(value: ValueModel) extends NoExecModel {
  override def toString: String = s"declare $value"

  override def usesVarNames: Set[String] = value.usesVarNames
}

case class FlattenModel(value: ValueModel, assignTo: String) extends OpModel {
  override def usesVarNames: Set[String] = value.usesVarNames

  override def exportsVarNames: Set[String] = Set(assignTo)
}

case class PushToStreamModel(value: ValueModel, exportTo: CallModel.Export) extends OpModel {

  override def usesVarNames: Set[String] = value.usesVarNames

  override def exportsVarNames: Set[String] = Set(exportTo.name)
}

case class CallServiceModel(serviceId: ValueModel, funcName: String, call: CallModel)
    extends ForceExecModel {

  override def toString: String = s"(call _ ($serviceId $funcName) $call)"

  override lazy val usesVarNames: Set[String] = serviceId.usesVarNames ++ call.usesVarNames

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet
}

object CallServiceModel {

  def apply(
    serviceId: String,
    funcName: String,
    args: List[ValueModel],
    result: VarModel
  ): CallServiceModel =
    CallServiceModel(
      LiteralModel.quote(serviceId),
      funcName,
      CallModel(
        args,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    )
}

case class CanonicalizeModel(operand: ValueModel, exportTo: CallModel.Export)
    extends ForceExecModel {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def usesVarNames: Set[String] = operand.usesVarNames
}

case class JoinModel(operands: NonEmptyList[ValueModel]) extends ForceExecModel {

  override def toString: String = s"join ${operands.toList.mkString(", ")}"

  override lazy val usesVarNames: Set[String] =
    operands.toList.flatMap(_.usesVarNames).toSet
}

case class CaptureTopologyModel(name: String) extends NoExecModel
case class ApplyTopologyModel(name: String) extends SeqGroupModel

case object EmptyModel extends NoExecModel
case object NullModel extends NoExecModel
