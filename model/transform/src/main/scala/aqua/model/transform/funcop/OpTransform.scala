package aqua.model.transform.funcop

import aqua.model.OpModel

import cats.data.Chain
import cats.free.Cofree
import cats.Eval

trait OpTransform {
  def folder: OpTransform.OpFolder

  def apply(tree: OpModel.Tree): Eval[OpModel.Tree] =
    Cofree.cata[Chain, OpModel, OpModel.Tree](tree)((op, children) =>
      folder
        .lift(op, children)
        .getOrElse(
          Eval.now(
            op.wrap(children.toList: _*)
          )
        )
    )
}

object OpTransform {
  type OpFolder = PartialFunction[(OpModel, Chain[OpModel.Tree]), Eval[OpModel.Tree]]
}
