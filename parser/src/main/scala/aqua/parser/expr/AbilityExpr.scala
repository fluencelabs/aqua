package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, NamedTypeToken, Name, ValueToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class AbilityExpr[F[_]](name: NamedTypeToken[F]) extends Expr[F](AbilityExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): AbilityExpr[K] =
    copy(name.mapK(fk))
}

object AbilityExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = FieldTypeExpr :: ArrowTypeExpr :: Nil

  override val p: Parser[AbilityExpr[Span.S]] =
    (`ability` *> ` ` *> NamedTypeToken.ct).map(AbilityExpr(_))
}
