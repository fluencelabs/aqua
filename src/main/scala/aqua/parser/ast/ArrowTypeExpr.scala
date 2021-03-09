package aqua.parser.ast

import aqua.interim.abilities.AbilitiesAlgebra
import aqua.interim.types.TypesAlgebra
import aqua.parser.lexer.{ArrowName, ArrowTypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class ArrowTypeExpr[F[_]](name: ArrowName[F], `type`: ArrowTypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[Alg], A: AbilitiesAlgebra[Alg]): Prog[Alg, Unit] =
    for {
      t <- T.resolveArrowDef(`type`)
      _ <- A.defineArrow(name, t)
    } yield ()

}

object ArrowTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((ArrowName.an[F] <* ` : `) ~ ArrowTypeToken.`arrowdef`[F]).map {
      case (name, t) => ArrowTypeExpr(name, t)
    }
}
