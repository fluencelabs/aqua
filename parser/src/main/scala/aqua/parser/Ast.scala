package aqua.parser

import aqua.parser.expr._
import aqua.parser.head.{HeadExpr, HeaderExpr}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.data.{Chain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser0 as P0
import cats.{Comonad, Eval}
import cats.~>

case class Ast[S[_]](head: Ast.Head[S], tree: Ast.Tree[S]) {

  def cata[T](folder: (Expr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[S], T](tree)(folder)

  def cataHead[T](folder: (HeaderExpr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, HeaderExpr[S], T](head)(folder)
}

object Ast {
  type Tree[S[_]] = Cofree[Chain, Expr[S]]
  type Head[S[_]] = Cofree[Chain, HeaderExpr[S]]

  def parser[S[_]: LiftParser: Comonad](): P0[ValidatedNec[ParserError[S], Ast[S]]] =
    (HeadExpr.ast[S].with1 ~ RootExpr.ast[S]()).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }

  def fromString[K[_]: Comonad: LiftParser, S[_]: Comonad](parser: P0[ValidatedNec[ParserError[K], Ast[K]]], script: String, modify: K ~> S): ValidatedNec[ParserError[S], Ast[S]] =
    parser
      .parseAll(script) match {
      case Right(value) => value.bimap(
        e => e.map(_.mapK(modify)),
        ast => Ast[S](ast.head.map(_.mapK(modify)), ast.tree.map(_.mapK(modify)))
      )
      case Left(e) => Validated.invalidNec(LexerError[K](e.wrapErr).mapK(modify))
    }

}
