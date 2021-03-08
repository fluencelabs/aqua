package aqua.parser.ast

import aqua.parser.lift.LiftParser
import cats.free.Cofree
import cats.{Comonad, Eval}
import cats.parse.{Parser => P}
import cats.syntax.functor._
import aqua.parser.lexer.Token._

trait Expr[F[_]] {}

object Expr {

  trait Companion {
    def wrapsExprs: List[Companion] = Nil

    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def ast[F[_]: LiftParser: Comonad](ps: ParserState): P[Ast.Tree[F]] =
      wrapsExprs match {
        case Nil => p[F].map(Cofree[List, Expr[F]](_, Eval.now(Nil)))

        case cs =>
          (p[F] ~ indented(
            s => {
              val psI = ps.copy(indent = s)
              P.oneOf(cs.map(_.ast[F](psI)))
            },
            ps.indent
          )).map {
            case (expr, internal) => Cofree[List, Expr[F]](expr, Eval.now(internal.toList))
          }

      }
  }
}
