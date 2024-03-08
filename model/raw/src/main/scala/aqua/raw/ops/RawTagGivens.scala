package aqua.raw.ops

import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.{Eval, Semigroup}

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

    def exportedAndUsesNames: Eval[(Set[String], Set[String])] =
      Cofree
        .cata(tree)((tag, childs: Chain[(Set[String], Set[String])]) =>
          Eval.later {
            val (childExports, childUses) = childs.combineAll
            val exports = tag.exportsVarNames ++ childExports -- tag.restrictsVarNames
            val uses = tag.usesVarNames ++ childUses -- exports
            (exports, uses)
          }
        )

    /**
     * Get all variable names used by this tree
     * but not exported in it (free variables).
     */
    def usesVarNames: Eval[Set[String]] =
      exportedAndUsesNames
        .map { case (_, uses) => uses }

    def exportsVarNames: Eval[Set[String]] =
      exportedAndUsesNames
        .map { case (exports, _) => exports }

    private def collect[A](pf: PartialFunction[RawTag, A]): Eval[Chain[A]] =
      Cofree.cata(tree)((tag, acc: Chain[Chain[A]]) =>
        Eval.later(Chain.fromOption(pf.lift(tag)) ++ acc.flatten)
      )
}
