package aqua.semantics.expr.func

import aqua.parser.expr.func.ForkExpr
import aqua.parser.lexer.{Name, ValueToken}
import aqua.raw.Raw
import aqua.raw.ops.*
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.expr.func.FuncOpSem
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*

import cats.Monad
import cats.data.{Chain, OptionT}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*

class ForkSem[S[_]](val expr: ForkExpr[S]) extends AnyVal {

  def program[F[_]: Monad](using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): Prog[F, Raw] =
    Prog
      .around(
        ForSem.beforeFor(expr.item, expr.iterable),
        // Without type of ops specified
        // scala compiler fails to compile this
        (iterable, ops: Raw) =>
          (iterable, ops) match {
            case (Some(vm), FuncOp(op)) =>
              FuncOpSem.restrictStreamsInScope(op).map { restricted =>
                ParTag.Detach
                  .wrap(
                    ForkTag(expr.item.value, vm)
                      .wrap(
                        ParTag.wrap(
                          restricted,
                          NextTag(expr.item.value).leaf
                        )
                      )
                  )
                  .toFuncOp
              }
            case _ => Raw.error("Wrong body of the `fork` expression").pure[F]
          }
      )
      .namesScope(expr.token)
      .abilitiesScope(expr.token)
}
