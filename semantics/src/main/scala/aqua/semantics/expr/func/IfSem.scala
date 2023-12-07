package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, IfTag}
import aqua.parser.expr.func.IfExpr
import aqua.raw.value.ValueRaw
import aqua.raw.Raw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.Type

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.apply.*
import cats.syntax.traverse.*
import aqua.types.ScalarType

class IfSem[S[_]](val expr: IfExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        V.valueToRaw(expr.value)
          .flatMap(
            _.flatTraverse(raw =>
              T.ensureTypeMatches(
                token = expr.value,
                expected = ScalarType.bool,
                givenType = raw.`type`
              ).map(Option.when(_)(raw))
            )
          ),
        // Without type of ops specified
        // scala compiler fails to compile this
        (value, ops: Raw) =>
          (value, ops) match {
            case (Some(vr), FuncOp(op)) =>
              for {
                restricted <- FuncOpSem.restrictStreamsInScope(op)
                tag = IfTag(vr).wrap(restricted)
              } yield tag.toFuncOp
            case (None, _) => Raw.error("`if` expression errored in matching types").pure
            case _ => Raw.error("Wrong body of the `if` expression").pure
          }
      )
      .abilitiesScope[S](expr.token)
      .namesScope[S](expr.token)
}
