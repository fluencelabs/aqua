package aqua.model

import aqua.model.OpModel.Tree
import cats.data.Chain
import cats.free.Cofree
import cats.Show
import cats.Eval
import cats.data.NonEmptyList
import aqua.tree.TreeNode

import scala.annotation.tailrec

sealed trait OpModel extends TreeNode[OpModel] {

  // What var names are restricted only for children of this tag – CANNOT be used after this tag, only within
  def restrictsVarNames: Set[String] = Set.empty

  // What variable names this tag uses (children are not respected)
  def usesVarNames: Set[String] = Set.empty

  // What var names are exported – can be used AFTER this tag is executed
  def exportsVarNames: Set[String] = Set.empty

}

object OpModel extends OpModelShow {
  type Tree = Cofree[Chain, OpModel]

  def exportsVarNames(tree: Tree): Eval[Set[String]] = Cofree.cata(tree) { case (op, acc) =>
    Eval.later(acc.foldLeft(op.exportsVarNames)(_ ++ _) -- op.restrictsVarNames)
  }

  // TODO: as it is used for checking of intersection, make it a lazy traverse with fail-fast
  def usesVarNames(tree: Tree): Eval[Set[String]] = Cofree.cata(tree) { case (op, acc) =>
    Eval.later(acc.foldLeft(op.usesVarNames)(_ ++ _) -- op.restrictsVarNames)
  }
}

sealed trait NoExecModel extends OpModel

sealed trait GroupOpModel extends OpModel

sealed trait SeqGroupModel extends GroupOpModel

sealed trait ParGroupModel extends GroupOpModel

case object SeqModel extends SeqGroupModel {

  override def wrap(children: Tree*): Tree =
    super.wrapNonEmpty(children.filterNot(_.head == EmptyModel).toList, EmptyModel.leaf)
}

case object ParModel extends ParGroupModel

case object DetachModel extends ParGroupModel

case object XorModel extends GroupOpModel

case class OnModel(peerId: ValueModel, via: Chain[ValueModel]) extends SeqGroupModel {

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

  override def usesVarNames: Set[String] =
    left.usesVarNames ++ right.usesVarNames
}

case class ForModel(item: String, iterable: ValueModel) extends SeqGroupModel {

  override def restrictsVarNames: Set[String] = Set(item)

  override def usesVarNames: Set[String] = iterable.usesVarNames

}

case class DeclareStreamModel(value: ValueModel) extends NoExecModel {

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
    extends OpModel {

  override lazy val usesVarNames: Set[String] = serviceId.usesVarNames ++ call.usesVarNames

  override def exportsVarNames: Set[String] = call.exportTo.map(_.name).toSet
}

case class CanonicalizeModel(operand: ValueModel, exportTo: CallModel.Export) extends OpModel {

  override def exportsVarNames: Set[String] = Set(exportTo.name)

  override def usesVarNames: Set[String] = operand.usesVarNames
}

case class JoinModel(operands: NonEmptyList[ValueModel]) extends OpModel {

  override lazy val usesVarNames: Set[String] = operands.toList.flatMap(_.usesVarNames).toSet
}

case object EmptyModel extends NoExecModel
