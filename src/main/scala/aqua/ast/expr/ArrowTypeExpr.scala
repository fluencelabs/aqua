package aqua.ast.expr

import aqua.ast.{Expr, Prog}
import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{ArrowTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class ArrowTypeExpr[F[_]](name: Name[F], `type`: ArrowTypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg], A: AbilitiesAlgebra[F, Alg]): Prog[Alg, Unit] =
    for {
      t <- T.resolveArrowDef(`type`)
      _ <- A.defineArrow(name, t)
    } yield ()

}

object ArrowTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((Name.p[F] <* ` : `) ~ ArrowTypeToken.`arrowdef`[F]).map {
      case (name, t) => ArrowTypeExpr(name, t)
    }
}
