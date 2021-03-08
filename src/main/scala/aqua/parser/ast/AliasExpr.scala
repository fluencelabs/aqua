package aqua.parser.ast

import aqua.parser.lexer.{CustomTypeToken, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class AliasExpr[F[_]](name: CustomTypeToken[F], target: TypeToken[F]) extends Expr[F] {}

object AliasExpr extends Expr.Companion {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((`alias` *> ` ` *> CustomTypeToken.ct[F] <* ` : `) ~ TypeToken.`typedef`[F]).map {
      case (name, target) => AliasExpr(name, target)
    }
}
