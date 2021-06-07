package aqua.semantics.expr

import aqua.model.{Model, ServiceModel, ValueModel}
import aqua.parser.expr.ServiceExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._

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
            for {
              defaultId <- expr.id
                .map(v => V.valueToModel(v))
                .getOrElse(Free.pure[Alg, Option[ValueModel]](None))
              defineResult <- A.defineService(
                expr.name,
                arrows,
                defaultId
              )
              _ <- expr.id
                .fold(Free.pure[Alg, Unit](()))(idV =>
                  (V.ensureIsString(idV) >> A.setServiceId(expr.name, idV)).map(_ => ())
                )
            } yield
              if (defineResult) {
                ServiceModel(expr.name.value, arrows, defaultId)
              } else Model.empty("Service not created due to validation errors")

          case None =>
            Free.pure(Model.error("Service has no arrows, fails"))

        }
    )
}
