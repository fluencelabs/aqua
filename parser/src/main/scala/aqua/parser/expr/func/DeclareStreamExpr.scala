package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{DataTypeToken, Name, Token, TypeToken}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: DataTypeToken[F])
    extends Expr[F](DeclareStreamExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): DeclareStreamExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object DeclareStreamExpr extends Expr.Leaf {

  override val p: P[DeclareStreamExpr[Span.S]] =
    ((Name.p <* ` : `) ~ DataTypeToken.`datatypedef`).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
