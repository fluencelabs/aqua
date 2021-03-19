package aqua.semantics.expr

import aqua.generator.{ArrowGen, Gen}
import aqua.parser.expr.ServiceExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
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
  ): Prog[Alg, Gen] =
    Prog.around(
      A.beginScope(expr.name),
      (_: Unit, body: Gen) =>
        (A.purgeArrows(expr.name) <* A.endScope()).flatMap {
          case Some(nel) =>
            A.defineService(
              expr.name,
              nel.map(kv => kv._1.value -> ArrowGen.service(expr.name.value, kv._1.value, kv._2)).toNem
            ) >>
              expr.id.fold(Free.pure[Alg, Gen](Gen.noop))(idV =>
                V.ensureIsString(idV) >> A.setServiceId(expr.name, idV) as Gen.noop
              )
          case None =>
            Gen.error.lift

        }
    )
}
