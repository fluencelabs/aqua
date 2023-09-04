package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.ops.AbilityIdTag
import aqua.parser.expr.func.AbilityIdExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra

import cats.Monad
import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AbilityIdSem[S[_]](val expr: AbilityIdExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = (
    for {
      _ <- EitherT(
        V.ensureIsString(expr.id)
          .map(isString =>
            Raw
              .error("Service ID was not a string")
              .asLeft
              .whenA(!isString)
          )
      )
      id <- EitherT.fromOptionF(
        V.valueToRaw(expr.id),
        Raw.error("Can not resolve service ID")
      )
      _ <- EitherT.liftF(
        A.setServiceId(expr.ability, id)
      )
    } yield AbilityIdTag(
      id,
      expr.ability.value
    ).funcOpLeaf
  ).value.map(_.merge)
}
