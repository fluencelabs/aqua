package aqua.ast.expr

import aqua.ast.{Expr, Prog}
import aqua.ast.algebra.ValuesAlgebra
import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.free.Free
import cats.parse.Parser
import cats.syntax.apply._
import cats.syntax.comonad._
import cats.syntax.flatMap._

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]]) extends Expr[F] {

  def program[Alg[_]](implicit
    A: AbilitiesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    F: Comonad[F]
  ): Prog[Alg, Unit] =
    Prog.around(
      A.beginScope(name),
      (_: Unit) =>
        (A.purgeArrows() <* A.endScope())
          .map(_.map(kv => kv._1.name.extract -> kv._2).toNem)
          .flatMap { arrows =>
            A.defineService(name, arrows) >>
              id.fold(Free.pure[Alg, Unit](()))(idV => V.ensureIsString(idV) >> A.setServiceId(name, idV))

          }
    )

}

object ServiceExpr extends Expr.AndIndented(ArrowTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[ServiceExpr[F]] =
    (`service` *> ` ` *> Ability.ab[F] ~ Value.`value`[F].between(`(`, `)`).backtrack.? <* ` : \n+`).map {
      case (name, id) => ServiceExpr(name, id)
    }
}
