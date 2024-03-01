package aqua.model.transform.funcop

import aqua.model.OpModel

import cats.Eval
import cats.data.Chain
import cats.free.Cofree

/**
 * Base type for [[OpModel.Tree]] -> [[OpModel.Tree]] transformation
 */
trait OpTransform {

  /**
   * Transformation step
   * (node, child results) => node result
   */
  def folder: OpTransform.OpFolder

  def apply(tree: OpModel.Tree): Eval[OpModel.Tree] =
    Cofree.cata[Chain, OpModel, OpModel.Tree](tree)((op, children) =>
      folder
        .lift(op, children)
        .getOrElse(
          Eval.now(
            op.wrap(children.toList*)
          )
        )
    )
}

object OpTransform {
  type OpFolder = PartialFunction[(OpModel, Chain[OpModel.Tree]), Eval[OpModel.Tree]]
}
