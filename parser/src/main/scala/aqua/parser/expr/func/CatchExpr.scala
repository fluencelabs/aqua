package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{CatchExpr, TryExpr}
import aqua.parser.lexer.Name
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser
import cats.{Comonad, ~>}

case class CatchExpr[F[_]](name: Name[F]) extends Expr[F](CatchExpr, name) {
  def mapK[K[_]: Comonad](fk: F ~> K): CatchExpr[K] = copy(name.mapK(fk))
}

object CatchExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = TryExpr.validChildren

  override val p: Parser[Expr[Span.S]] =
    (`catch` *> ` ` *> Name.p).map(CatchExpr(_))

}
