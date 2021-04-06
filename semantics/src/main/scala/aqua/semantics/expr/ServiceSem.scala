package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.ServiceExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

class ServiceSem[F[_]](val expr: ServiceExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    A: AbilitiesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      A.beginScope(expr.name),
      (_: Unit, body: Model) =>
        (A.purgeArrows(expr.name) <* A.endScope()).flatMap {
          case Some(nel) =>
            A.defineService(
              expr.name,
              nel.map(kv => kv._1.value -> kv._2).toNem
            ) >>
              expr.id.fold(Free.pure[Alg, Model](Model.empty("No service id is OK")))(idV =>
                V.ensureIsString(idV) >> A.setServiceId(expr.name, idV) as Model.empty("Service with ID defined")
              )
          case None =>
            Free.pure(Model.error("Service has no arrows, fails"))

        }
    )
}
