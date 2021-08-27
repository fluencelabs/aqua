package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.~>

case class PushToStreamExpr[F[_]](
  stream: Name[F],
  value: Value[F]
) extends Expr[F](PushToStreamExpr, stream) {

  override def mapK[K[_]: Comonad](fk: F ~> K): PushToStreamExpr[K] =
    copy(stream.mapK(fk), value.mapK(fk))
}

object PushToStreamExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[PushToStreamExpr[F]] =
    ((Name.p[F] <* ` <<- `).with1 ~ Value.`value`).map { case (variable, value) =>
      PushToStreamExpr(variable, value)
    }
}
