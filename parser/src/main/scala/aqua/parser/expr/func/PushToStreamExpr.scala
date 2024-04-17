package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class PushToStreamExpr[F[_]](
  stream: Name[F],
  value: ValueToken[F]
) extends Expr[F](PushToStreamExpr, stream) {

  override def mapK[K[_]: Comonad](fk: F ~> K): PushToStreamExpr[K] =
    copy(stream.mapK(fk), value.mapK(fk))
}

object PushToStreamExpr extends Expr.Leaf {

  override val p: P[PushToStreamExpr[Span.S]] =
    ((Name.p <* ` <<- `).with1 ~ ValueToken.`value`).map { case (variable, value) =>
      PushToStreamExpr(variable, value)
    }
}
