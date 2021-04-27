package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.{Name, StreamTypeToken, Token}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import Token._

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: StreamTypeToken[F]) extends Expr[F]

object DeclareStreamExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[DeclareStreamExpr[F]] =
    ((Name.p[F] <* ` : `) ~ StreamTypeToken.`streamtypedef`[F]).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
