package aqua.raw.ops

import aqua.raw.value.{LiteralRaw, ValueRaw}

import cats.free.Cofree
import cats.data.Chain
import cats.{Eval, Semigroup}
import cats.syntax.apply.*
import cats.syntax.semigroup.*

trait RawTagGivens {

  given Semigroup[RawTag.Tree] with

    override def combine(x: RawTag.Tree, y: RawTag.Tree): RawTag.Tree = {
      // Remove seq with single child
      val flatX = SeqGroupTag.ungroupSingle(x)
      val flatY = SeqGroupTag.ungroupSingle(y)
      (flatX.head, flatY.head) match {
        case (SeqTag, SeqTag) => flatX.copy(tail = (flatX.tail, flatY.tail).mapN(_ ++ _))
        case (_, SeqTag) => flatY.copy(tail = flatY.tail.map(_.prepend(flatX)))
        case (SeqTag, _) => flatX.copy(tail = flatX.tail.map(_.append(flatY)))
        case _ => SeqTag.wrap(flatX, flatY)
      }
    }

  extension (tree: RawTag.Tree)

    def toFuncOp: FuncOp = FuncOp(tree)

    def rename(vals: Map[String, String]): RawTag.Tree =
      if (vals.isEmpty) tree
      else tree.map(_.mapValues(_.renameVars(vals)).renameExports(vals))

    def mapValues(f: ValueRaw => ValueRaw): RawTag.Tree =
      tree.map(_.mapValues(f))

    def renameExports(vals: Map[String, String]): RawTag.Tree =
      if (vals.isEmpty) tree
      else tree.map(_.renameExports(vals))

    def definesVarNames: Eval[Set[String]] =
      Cofree.cata(tree) { case (tag, acc) =>
        Eval.later(acc.foldLeft(tag.definesVarNames)(_ ++ _))
      }
}
