package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.Expr.RootCompanion
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{CustomTypeToken, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class AliasExpr[F[_]](name: CustomTypeToken[F], target: TypeToken[F]) extends Expr[F]

object AliasExpr extends Expr.Leaf with RootCompanion {

  override def p[F[_]: LiftParser: Comonad]: Parser[AliasExpr[F]] =
    ((`alias` *> ` ` *> CustomTypeToken.ct[F] <* ` : `) ~ TypeToken.`typedef`[F]).map {
      case (name, target) =>
        AliasExpr(name, target)
    }
}
