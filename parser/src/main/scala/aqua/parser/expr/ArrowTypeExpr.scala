package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{ArrowTypeToken, BasicTypeToken, Name}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ArrowTypeExpr[F[_]](name: Name[F], `type`: ArrowTypeToken[F])
    extends Expr[F](ArrowTypeExpr, name) {
  def mapK[K[_]: Comonad](fk: F ~> K): ArrowTypeExpr[K] = copy(name.mapK(fk), `type`.mapK(fk))
}

object ArrowTypeExpr extends Expr.Leaf {

  override val p: Parser[ArrowTypeExpr[Span.S]] =
    (Name.p ~ ((` : ` *> ArrowTypeToken.`arrowdef`(
      BasicTypeToken.`compositetypedef`
    )) | ArrowTypeToken.`arrowWithNames`(BasicTypeToken.`compositetypedef`))).map {
      case (name, t) =>
        ArrowTypeExpr(name, t)
    }
}
