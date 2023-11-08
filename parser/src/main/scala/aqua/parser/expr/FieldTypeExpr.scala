package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{CompositeTypeToken, Name, StreamTypeToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class FieldTypeExpr[F[_]](name: Name[F], `type`: CompositeTypeToken[F])
    extends Expr[F](FieldTypeExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): FieldTypeExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object FieldTypeExpr extends Expr.Leaf {

  override val p: Parser[FieldTypeExpr[Span.S]] =
    ((Name.p <* ` : `) ~ CompositeTypeToken.`compositetypedef`).map { case (name, t) =>
      FieldTypeExpr(name, t)
    }
}
