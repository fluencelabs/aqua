package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{BasicTypeToken, Name, Token, TypeToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: BasicTypeToken[F])
    extends Expr[F](DeclareStreamExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): DeclareStreamExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object DeclareStreamExpr extends Expr.Leaf {

  override val p: P[DeclareStreamExpr[Span.S]] =
    ((Name.p <* ` : `) ~ BasicTypeToken.`compositetypedef`).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
