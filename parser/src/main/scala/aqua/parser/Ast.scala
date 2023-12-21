package aqua.parser

import aqua.helpers.tree.Tree
import aqua.parser.head.HeaderExpr
import aqua.parser.lexer.Token

import cats.data.Chain
import cats.free.Cofree
import cats.syntax.flatMap.*
import cats.syntax.show.*
import cats.{Comonad, ~>}
import cats.{Eval, Show}

case class Ast[S[_]](head: Ast.Head[S], tree: Ast.Tree[S]) {

  def mapK[K[_]: Comonad](nt: S ~> K): Ast[K] =
    Ast(head.mapK(nt), tree.map(_.mapK(nt)))

  def cata[T](folder: (Expr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[S], T](tree)(folder)
}

object Ast {

  final case class Head[S[_]](
    begin: Token[S],
    headers: Chain[HeaderExpr[S]]
  ) {

    def mapK[K[_]: Comonad](nt: S ~> K): Head[K] =
      copy(
        begin = begin.mapK(nt),
        headers = headers.map(_.mapK(nt))
      )

    def collect[T](pf: PartialFunction[HeaderExpr[S], T]): Chain[T] =
      headers.collect(pf)
  }

  type Tree[S[_]] = Cofree[Chain, Expr[S]]

  given [S[_]]: Show[Ast[S]] with {

    def show(ast: Ast[S]): String = {
      val head = ast.head.headers.map(_.show).toList.mkString("\n")
      val body = Tree.show(ast.tree)

      s"$head\n$body"
    }
  }
}
