package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.Parser as P

trait HeaderExpr[S[_]] {
  def token: Token[S]
}

object HeaderExpr {

  trait Companion {
    def p[S[_]: LiftParser: Comonad]: P[HeaderExpr[S]]

    def ast[S[_]: LiftParser: Comonad]: P[Ast.Head[S]]
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad]: P[Ast.Head[F]] =
      p[F].map(Cofree[Chain, HeaderExpr[F]](_, Eval.now(Chain.empty)))
  }
}
