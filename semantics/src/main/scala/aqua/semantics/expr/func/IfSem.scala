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

class IfSem[S[_]](val expr: IfExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        (V.valueToRaw(expr.left), V.valueToRaw(expr.right)).flatMapN {
          case (Some(lt), Some(rt)) =>
            T.ensureValuesComparable(
              token = expr.token,
              left = lt.`type`,
              right = rt.`type`
            ).map(Option.when(_)(lt -> rt))
          case _ => None.pure
        },
        (values: Option[(ValueRaw, ValueRaw)], ops: Raw) =>
          values
            .fold(
              Raw.error("`if` expression errored in matching types")
            ) { case (lt, rt) =>
              ops match {
                case FuncOp(op) =>
                  IfTag(
                    left = lt,
                    right = rt,
                    equal = expr.eqOp.value
                  ).wrap(op).toFuncOp
                case _ => Raw.error("Wrong body of the `if` expression")
              }
            }
            .pure
      )
      .abilitiesScope[S](expr.token)
      .namesScope[S](expr.token)
      .locationsScope()
}
