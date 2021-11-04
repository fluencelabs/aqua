package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.Parser as P
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

trait HeaderExpr[S[_]] {
  def token: Token[S]
  
  def mapK[K[_]: Comonad](fk: S ~> K): HeaderExpr[K]
}

object HeaderExpr {

  trait Companion {
    def p: P[HeaderExpr[Span.S]]

    def ast: P[Ast.Head[Span.S]]
  }

  abstract class Leaf extends Companion {

    override def ast: P[Ast.Head[Span.S]] =
      p.map(Cofree[Chain, HeaderExpr[Span.S]](_, Eval.now(Chain.empty)))
  }
}
