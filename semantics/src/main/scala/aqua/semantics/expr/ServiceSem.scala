package aqua.semantics.expr

import aqua.model.{Model, ServiceModel, ValueModel}
import aqua.parser.expr.ServiceExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.Monad

class ServiceSem[S[_]](val expr: ServiceExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
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
                .getOrElse(None.pure[Alg])
              defineResult <- A.defineService(
                expr.name,
                arrows,
                defaultId
              )
              _ <- (expr.id zip defaultId)
                .fold(().pure[Alg])(idV =>
                  (V.ensureIsString(idV._1) >> A.setServiceId(expr.name, idV._1, idV._2)).map(_ =>
                    ()
                  )
                )
            } yield
              if (defineResult) {
                ServiceModel(expr.name.value, arrows, defaultId)
              } else Model.empty("Service not created due to validation errors")

          case None =>
            Model.error("Service has no arrows, fails").pure[Alg]

        }
    )
}
