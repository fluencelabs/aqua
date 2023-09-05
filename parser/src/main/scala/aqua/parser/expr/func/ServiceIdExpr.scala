package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{NamedTypeToken, ValueToken}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ServiceIdExpr[F[_]](service: NamedTypeToken[F], id: ValueToken[F])
    extends Expr[F](ServiceIdExpr, service) {

  def mapK[K[_]: Comonad](fk: F ~> K): ServiceIdExpr[K] =
    copy(service.copy(fk(service.name)), id.mapK(fk))

}

object ServiceIdExpr extends Expr.Leaf {

  override val p: P[ServiceIdExpr[Span.S]] =
    ((NamedTypeToken.dotted <* ` `) ~ ValueToken.`value`).map { case (ability, id) =>
      ServiceIdExpr(ability, id)
    }

}
