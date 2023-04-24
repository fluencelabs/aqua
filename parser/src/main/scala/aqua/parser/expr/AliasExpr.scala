package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{NamedTypeToken, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan}

case class AliasExpr[F[_]](name: NamedTypeToken[F], target: TypeToken[F])
    extends Expr[F](AliasExpr, name) {
  def mapK[K[_]: Comonad](fk: F ~> K): AliasExpr[K] = copy(name.mapK(fk), target.mapK(fk))
}

object AliasExpr extends Expr.Leaf {

  override val p: Parser[AliasExpr[Span.S]] =
    ((`alias` *> ` ` *> NamedTypeToken.ct <* ` : `) ~ TypeToken.`typedef`).map {
      case (name, target) =>
        AliasExpr(name, target)
    }
}
