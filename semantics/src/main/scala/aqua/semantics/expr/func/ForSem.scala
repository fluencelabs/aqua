package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.parser.expr.func.ForExpr
import aqua.raw.value.ValueRaw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag.WaitMode
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType}

import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ForSem[S[_]](val expr: ForExpr[S]) extends AnyVal {

  def program[F[_]: Monad](implicit
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): Prog[F, Raw] =
    Prog
      .around(
        V.valueToRaw(expr.iterable).flatMap[Option[ValueRaw]] {
          case Some(vm) =>
            vm.`type` match {
              case t: BoxType =>
                N.define(expr.item, t.element).as(Option(vm))
              case dt =>
                T.ensureTypeMatches(expr.iterable, ArrayType(dt), dt).as(Option.empty[ValueRaw])
            }

          case _ => None.pure[F]
        },
        (stOpt: Option[ValueRaw], ops: Raw) =>
          N.streamsDefinedWithinScope()
            .map(_.keySet)
            .map(streams =>
              (stOpt, ops) match {
                case (Some(vm), FuncOp(op)) =>
                  val innerTag = expr.mode.fold[RawTag](SeqTag) {
                    case ForExpr.Mode.ParMode => ParTag
                    case ForExpr.Mode.TryMode => TryTag
                  }

                  val mode = expr.mode.collect { case ForExpr.Mode.ParMode => WaitMode }

                  val forTag =
                    ForTag(expr.item.value, vm, mode).wrap(
                      innerTag
                        .wrap(
                          // Restrict the streams created within this scope
                          streams.toList.foldLeft(op) { case (b, streamName) =>
                            RestrictionTag(streamName, isStream = true).wrap(b)
                          },
                          NextTag(expr.item.value).leaf
                        )
                    )

                  // Fix: continue execution after fold par immediately, without finding a path out from par branches
                  if (innerTag == ParTag) ParTag.Detach.wrap(forTag).toFuncOp
                  else forTag.toFuncOp
                case _ =>
                  Raw.error("Wrong body of the `for` expression")
              }
            )
      )
      .namesScope[S](expr.token)
      .abilitiesScope[S](expr.token)
}
