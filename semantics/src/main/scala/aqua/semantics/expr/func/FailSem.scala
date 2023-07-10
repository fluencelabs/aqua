package aqua.semantics.expr.func

import aqua.parser.expr.func.FailExpr
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.Prog
import aqua.raw.Raw
import aqua.types.ScalarType
import aqua.raw.ops.FailTag

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class FailSem[S[_]](val expr: FailExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = for {
    maybeValue <- V.valueToRaw(expr.value)
    result <- maybeValue.traverse(vr =>
      T.ensureTypeMatches(
        token = expr.value,
        expected = ScalarType.string,
        givenType = vr.`type`
      ).map(isStr =>
        if (isStr) FailTag(vr).funcOpLeaf
        else Raw.error("Argument of `fail` is not a string")
      )
    )
  } yield result.getOrElse(
    Raw.error("Resolution for `fail` argument failed")
  )
}
