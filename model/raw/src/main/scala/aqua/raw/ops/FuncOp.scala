package aqua.raw.ops

import aqua.raw.Raw
import aqua.raw.ops
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.instances.tuple.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.Monad
import cats.data.State
import cats.data.StateT

case class FuncOp(tree: RawTag.Tree) extends Raw {
  def isRightAssoc: Boolean = RawTag.isRightAssoc(tree.head)

  def :+:(prev: FuncOp): FuncOp = FuncOp(RawTag.rightAssocCombine(prev.tree, tree))
}
