package aqua.parser

import aqua.parser.expr._
import aqua.parser.head.{HeadExpr, HeaderExpr}
import aqua.parser.lift.LiftParser
import cats.data.{Chain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser0 => P0}
import cats.{Comonad, Eval}

case class Ast[F[_]](head: Ast.Head[F], tree: Ast.Tree[F]) {

  def cata[T](folder: (Expr[F], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[F], T](tree)(folder)
}

object Ast {
  type Tree[F[_]] = Cofree[Chain, Expr[F]]
  type Head[F[_]] = Cofree[Chain, HeaderExpr[F]]

  def parser[F[_]: LiftParser: Comonad](): P0[ValidatedNec[ParserError[F], Ast[F]]] =
    (HeadExpr.ast[F].with1 ~ RootExpr.ast[F]()).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }

  def fromString[F[_]: LiftParser: Comonad](script: String): ValidatedNec[ParserError[F], Ast[F]] =
    parser[F]()
      .parseAll(script) match {
      case Right(value) => value
      case Left(e) => Validated.invalidNec(LexerError[F](e))
    }

}
