package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.CustomTypeToken
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class DataStructExpr[F[_]](name: CustomTypeToken[F]) extends Expr[F](DataStructExpr, name) {
  override def mapK[K[_]: Comonad](fk: F ~> K): DataStructExpr[K] = copy(name.mapK(fk))
}

object DataStructExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = FieldTypeExpr :: Nil

  override val p: Parser[DataStructExpr[Span.S]] =
    `data` *> ` ` *> CustomTypeToken.ct.map(DataStructExpr(_))
}
