package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{PropertyToken, ValueToken}
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.{Comonad, ~>}

case class JoinExpr[F[_]](values: NonEmptyList[ValueToken[F]])
    extends Expr[F](JoinExpr, values.head) {

  override def mapK[K[_]: Comonad](fk: F ~> K): JoinExpr[K] =
    copy(values.map(_.mapK(fk)))
}

object JoinExpr extends Expr.Leaf {

  override val p: Parser[JoinExpr[Span.S]] =
    (`join` *> ` ` *> comma(PropertyToken.property)).map(JoinExpr(_))
}
