package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, MatchMismatchTag, XorTag}
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
        V.valueToRaw(expr.left).flatMap {
          case Some(lt) =>
            V.valueToRaw(expr.right).flatMap {
              case Some(rt) =>
                T.ensureValuesComparable(expr.right, lt.`type`, rt.`type`)
                  .map(m => Some(lt -> rt).filter(_ => m))
              case None =>
                None.pure[Alg]
            }
          case None =>
            V.resolveType(expr.right).as[Option[(ValueRaw, ValueRaw)]](None)
        },
        (r: Option[(ValueRaw, ValueRaw)], ops: Raw) =>
          r.fold(Raw.error("If expression errored in matching types").pure[Alg]) { case (lt, rt) =>
            ops match {
              case FuncOp(op) =>
                XorTag.LeftBiased
                  .wrap(
                    MatchMismatchTag(
                      lt,
                      rt,
                      expr.eqOp.value
                    ).wrap(op)
                  )
                  .toFuncOp
                  .pure[Alg]

              case _ => Raw.error("Wrong body of the if expression").pure[Alg]
            }
          }
      )
      .abilitiesScope[S](expr.token)
      .namesScope[S](expr.token)
      .locationsScope()
}
