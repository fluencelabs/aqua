package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.lexer.ValueToken
import aqua.parser.lift.Span.S
import aqua.parser.lexer.Token.*

import cats.Comonad
import cats.arrow.FunctionK
import cats.parse.Parser

final case class FailExpr[F[_]](
  value: ValueToken[F]
) extends Expr[F](FailExpr, value) {

  override def mapK[K[_]: Comonad](fk: FunctionK[F, K]): Expr[K] =
    FailExpr[K](value.mapK(fk))

}

object FailExpr extends Expr.Leaf {

  override def p: Parser[Expr[S]] =
    (`fail` *> ` ` *> ValueToken.`value`).map(FailExpr.apply)

}
