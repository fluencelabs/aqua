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

case class ScopeExpr[F[_]](name: NamedTypeToken[F]) extends Expr[F](ScopeExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ScopeExpr[K] =
    copy(name.mapK(fk))
}

object ScopeExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = FieldTypeExpr :: ArrowTypeExpr :: Nil

  override val p: Parser[ScopeExpr[Span.S]] =
    (`scope` *> ` ` *> NamedTypeToken.ct).map(ScopeExpr(_))
}
