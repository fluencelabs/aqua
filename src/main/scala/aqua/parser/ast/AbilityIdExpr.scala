package aqua.parser.ast

import aqua.interim.ValuesAlgebra
import aqua.interim.abilities.AbilitiesAlgebra
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad
import cats.syntax.flatMap._

case class AbilityIdExpr[F[_]](ability: Ability[F], id: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit A: AbilitiesAlgebra[Alg], V: ValuesAlgebra[Alg]): Prog[Alg, Unit] =
    V.ensureIsString(id) >> A.setServiceId(ability, id)

}

object AbilityIdExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AbilityIdExpr[F]] =
    ((Ability.ab[F] <* ` `) ~ Value.`value`).map {
      case (ability, id) => AbilityIdExpr(ability, id)
    }

}
