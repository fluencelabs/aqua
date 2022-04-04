package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{ArrowTypeToken, DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ArrowTypeExpr[F[_]](name: Name[F], `type`: ArrowTypeToken[F])
    extends Expr[F](ArrowTypeExpr, name) {
  def mapK[K[_]: Comonad](fk: F ~> K): ArrowTypeExpr[K] = copy(name.mapK(fk), `type`.mapK(fk))
}

object ArrowTypeExpr extends Expr.Leaf {

  override val p: Parser[ArrowTypeExpr[Span.S]] =
    (Name.p ~ ((` : ` *> ArrowTypeToken.`arrowdef`(
      DataTypeToken.`datatypedef`
    )) | ArrowTypeToken.`arrowWithNames`(DataTypeToken.`datatypedef`))).flatMap { case (name, t) =>
      // services cannot return multiple results
      if (t.res.length > 1) {
        Parser.failWith("Service functions cannot have multiple results")
      } else {
        Parser.pure(ArrowTypeExpr(name, t))
      }
    }
}
