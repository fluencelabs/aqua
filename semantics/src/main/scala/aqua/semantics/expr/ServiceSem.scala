package aqua.semantics.expr

import aqua.parser.expr.ServiceExpr
import aqua.raw.{Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.Monad

class ServiceSem[S[_]](val expr: ServiceExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.around(
      A.beginScope(expr.name),
      (_: Unit, body: Raw) =>
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
                ServiceRaw(expr.name.value, arrows, defaultId)
              } else Raw.empty("Service not created due to validation errors")

          case None =>
            Raw.error("Service has no arrows, fails").pure[Alg]

        }
    )
}
