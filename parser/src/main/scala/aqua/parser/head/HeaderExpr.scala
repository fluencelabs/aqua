package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P}

trait HeaderExpr[F[_]]

object HeaderExpr {

  trait Companion {
    def p[F[_]: LiftParser: Comonad]: P[HeaderExpr[F]]

    def ast[F[_]: LiftParser: Comonad]: P[Ast.Head[F]]
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad]: P[Ast.Head[F]] =
      p[F].map(Cofree[Chain, HeaderExpr[F]](_, Eval.now(Chain.empty)))
  }
}
