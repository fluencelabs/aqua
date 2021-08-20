package aqua.parser

import aqua.parser.expr._
import aqua.parser.head.{HeadExpr, HeaderExpr}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.data.{Chain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser0 as P0
import cats.{Comonad, Eval}

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

  def fromString[S[_]: LiftParser: Comonad](script: String): ValidatedNec[ParserError[S], Ast[S]] =
    parser[S]()
      .parseAll(script) match {
      case Right(value) => value
      case Left(e) => Validated.invalidNec(LexerError[S](e.wrapErr))
    }

}
