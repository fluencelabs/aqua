package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class PushToStreamExpr[F[_]](
  stream: Name[F],
  value: Value[F]
) extends Expr[F](PushToStreamExpr, stream) {

  override def mapK[K[_]: Comonad](fk: F ~> K): PushToStreamExpr[K] =
    copy(stream.mapK(fk), value.mapK(fk))
}

object PushToStreamExpr extends Expr.Leaf {

  override val p: P[PushToStreamExpr[Span.F]] =
    ((Name.p <* ` <<- `).with1 ~ Value.`value`).map { case (variable, value) =>
      PushToStreamExpr(variable, value)
    }
}
