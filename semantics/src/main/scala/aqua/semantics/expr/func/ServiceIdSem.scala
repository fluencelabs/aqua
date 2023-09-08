package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.ops.ServiceIdTag
import aqua.parser.expr.func.ServiceIdExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.rules.names.NamesAlgebra

import cats.Monad
import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ServiceIdSem[S[_]](val expr: ServiceIdExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    A: AbilitiesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = (
    for {
      id <- EitherT.fromOptionF(
        V.valueToStringRaw(expr.id),
        Raw.error("Can not resolve service ID")
      )
      serviceType <- EitherT.fromOptionF(
        T.resolveServiceType(expr.service),
        Raw.error("Can not resolve service type")
      )
      name <- EitherT.fromOptionF(
        A.setServiceId(expr.service, id),
        Raw.error("Can not set service ID")
      )
      _ <- EitherT.liftF(
        N.derive(
          expr.service.asName.rename(name),
          serviceType,
          id.varNames
        )
      )
    } yield ServiceIdTag(id, serviceType, name).funcOpLeaf
  ).value.map(_.merge)
}
