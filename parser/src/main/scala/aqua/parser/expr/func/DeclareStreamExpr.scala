package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, Token, TypeToken}
import aqua.parser.lift.LiftParser
import cats.parse.Parser
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: TypeToken[F])
    extends Expr[F](DeclareStreamExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): DeclareStreamExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object DeclareStreamExpr extends Expr.Leaf {

  override val p: Parser[DeclareStreamExpr[Span.F]] =
    ((Name.p <* ` : `) ~ TypeToken.`typedef`).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
