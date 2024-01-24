package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.Show
import cats.data.Chain
import cats.free.Cofree
import cats.parse.Parser as P
import cats.{Comonad, Eval}
import cats.~>

trait HeaderExpr[S[_]] {
  def token: Token[S]

  def mapK[K[_]: Comonad](fk: S ~> K): HeaderExpr[K]
}

object HeaderExpr {

  trait Companion {
    def p: P[HeaderExpr[Span.S]]
  }

  given [S[_]]: Show[HeaderExpr[S]] with {
    // TODO: Make it better
    def show(e: HeaderExpr[S]): String = e.toString
  }
}
