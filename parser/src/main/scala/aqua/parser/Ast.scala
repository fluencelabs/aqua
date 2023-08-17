package aqua.parser

import aqua.parser.expr.*
import aqua.parser.head.{HeadExpr, HeaderExpr}
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.lift.LiftParser.*
import aqua.helpers.Tree

import cats.data.{Chain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.{Comonad, Eval}
import cats.~>
import cats.Show

case class Ast[S[_]](head: Ast.Head[S], tree: Ast.Tree[S]) {

  def cata[T](folder: (Expr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[S], T](tree)(folder)

  def cataHead[T](folder: (HeaderExpr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, HeaderExpr[S], T](head)(folder)
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
