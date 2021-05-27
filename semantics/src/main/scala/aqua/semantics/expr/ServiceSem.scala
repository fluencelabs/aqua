package aqua.semantics.expr

import aqua.model.{Model, ServiceModel}
import aqua.parser.expr.ServiceExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.ScalarType
import cats.free.Free
import cats.syntax.apply._
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
            val arrows = nel.map(kv => kv._1.value -> kv._2).toNem
            expr.id.fold({
              A.defineService(
                expr.name,
                arrows,
                None
              ).flatMap {
                case true =>
                  Free.pure(ServiceModel(expr.name.value, arrows, None): Model)
                case false =>
                  Free.pure(Model.empty("Service not created due to validation errors"))
              }
            }) { idV =>
              V.ensureIsString(idV).flatMap {
                case false =>
                  Free.pure(Model.error(s"Service '${expr.name.value}' should use string id"))
                case true =>
                  val idVm = ValuesAlgebra.valueToModel(idV, ScalarType.string)

                  A.defineService(
                    expr.name,
                    arrows,
                    Some(idVm)
                  ).flatMap {
                    case true =>
                      A.setServiceId(expr.name, idV) as (ServiceModel(
                        expr.name.value,
                        arrows,
                        Some(idVm)
                      ): Model)
                    case false =>
                      Free.pure(Model.empty("Service not created due to validation errors"))
                  }

              }
            }

          case None =>
            Free.pure(Model.error("Service has no arrows, fails"))

        }
    )
}
