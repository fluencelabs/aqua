package aqua.parser.expr

import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.generator.Gen
import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.flatMap._
import cats.syntax.functor._

case class AbilityIdExpr[F[_]](ability: Ability[F], id: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit A: AbilitiesAlgebra[F, Alg], V: ValuesAlgebra[F, Alg]): Prog[Alg, Gen] =
    V.ensureIsString(id) >> A.setServiceId(ability, id) as Gen.noop

}

object AbilityIdExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AbilityIdExpr[F]] =
    ((Ability.ab[F] <* ` `) ~ Value.`value`).map {
      case (ability, id) => AbilityIdExpr(ability, id)
    }

}
