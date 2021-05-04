package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.CustomTypeToken
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class DataStructExpr[F[_]](name: CustomTypeToken[F]) extends Expr[F]

object DataStructExpr extends Expr.AndIndented {

  override def p[F[_]: LiftParser: Comonad]: Parser[DataStructExpr[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F].map(DataStructExpr(_))

  override def validChildren: List[Expr.Companion] = List(FieldTypeExpr)
}
