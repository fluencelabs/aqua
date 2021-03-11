package aqua.ast.expr

import aqua.ast.{Expr, Prog}
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{CustomTypeToken, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class AliasExpr[F[_]](name: CustomTypeToken[F], target: TypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Unit] =
    for {
      t <- T.resolveType(target)
      _ <- T.defineAlias(name, t)
    } yield ()

}

object AliasExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((`alias` *> ` ` *> CustomTypeToken.ct[F] <* ` : `) ~ TypeToken.`typedef`[F]).map {
      case (name, target) => AliasExpr(name, target)
    }
}
