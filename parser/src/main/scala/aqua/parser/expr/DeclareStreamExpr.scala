package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.{DataTypeToken, StreamName, Token}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import Token._

case class DeclareStreamExpr[F[_]](name: StreamName[F], `type`: DataTypeToken[F]) extends Expr[F]

object DeclareStreamExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[DeclareStreamExpr[F]] =
    ((StreamName.p[F] <* ` : `) ~ DataTypeToken.`datatypedef`[F]).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
