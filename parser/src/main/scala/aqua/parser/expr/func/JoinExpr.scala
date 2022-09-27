package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{ValueToken, VarToken}
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}
import cats.parse.Parser
import cats.{~>, Comonad}
import cats.data.NonEmptyList

case class JoinExpr[F[_]](values: NonEmptyList[VarToken[F]])
    extends Expr[F](JoinExpr, values.head) {

  override def mapK[K[_]: Comonad](fk: F ~> K): JoinExpr[K] =
    copy(values.map(_.mapK(fk)))
}

object JoinExpr extends Expr.Leaf {

  override val p: Parser[JoinExpr[Span.S]] =
    (`join` *> ` ` *> comma(ValueToken.varProperty)).map(JoinExpr(_))
}
