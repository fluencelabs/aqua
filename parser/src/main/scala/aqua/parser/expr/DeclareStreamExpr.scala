package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Token, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: TypeToken[F])
    extends Expr[F](DeclareStreamExpr, name)

object DeclareStreamExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[DeclareStreamExpr[F]] =
    ((Name.p[F] <* ` : `) ~ TypeToken.`typedef`[F]).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
