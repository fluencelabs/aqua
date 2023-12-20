package aqua.parser

import aqua.helpers.tree.Tree
import aqua.parser.head.HeaderExpr

import cats.data.Chain
import cats.free.Cofree
import cats.syntax.flatMap.*
import cats.{Eval, Show}

case class Ast[S[_]](head: Ast.Head[S], tree: Ast.Tree[S]) {

  def cata[T](folder: (Expr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[S], T](tree)(folder)

  def cataHead[T](folder: (HeaderExpr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, HeaderExpr[S], T](head)(folder)

  def collectHead[T](pf: PartialFunction[HeaderExpr[S], T]): Eval[Chain[T]] =
    cataHead((e, acc: Chain[Chain[T]]) =>
      Eval.later {
        val flatAcc = acc.flatten
        if (pf.isDefinedAt(e)) flatAcc :+ pf(e) else flatAcc
      }
    )
}

object Ast {
  type Head[S[_]] = Cofree[Chain, HeaderExpr[S]]
  type Tree[S[_]] = Cofree[Chain, Expr[S]]

  given [S[_]]: Show[Ast[S]] with {

    def show(ast: Ast[S]): String = {
      val head = Tree.show(ast.head)
      val body = Tree.show(ast.tree)

      s"$head\n$body"
    }
  }
}
