package aqua.parser.ast

import aqua.parser.lexer.CustomTypeToken
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class DataStructExpr[F[_]](name: CustomTypeToken[F]) extends Expr[F] {}

object DataStructExpr extends Expr.Companion {

  override def wrapsExprs: List[Expr.Companion] = FieldTypeExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[DataStructExpr[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F].map(DataStructExpr(_)) <* ` : \n+`
}
