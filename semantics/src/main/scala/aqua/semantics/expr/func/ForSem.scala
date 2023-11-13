package aqua.semantics.expr.func

import aqua.parser.expr.func.ForExpr
import aqua.parser.expr.func.ForExpr.Mode
import aqua.parser.lexer.{Name, ValueToken}
import aqua.raw.Raw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag
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

class ForSem[S[_]](val expr: ForExpr[S]) extends AnyVal {

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
                val innerTag = expr.mode.fold(SeqTag) {
                  case ForExpr.Mode.ParMode => ParTag
                  case ForExpr.Mode.TryMode => TryTag
                }

                /**
                 * `for ... par` => blocking (`never` as `last` in `fold`)
                 * `for` and `for ... try` => non blocking (`null` as `last` in `fold`)
                 */
                val mode = expr.mode.fold(ForTag.Mode.NonBlocking) {
                  case ForExpr.Mode.ParMode => ForTag.Mode.Blocking
                  case Mode.TryMode => ForTag.Mode.NonBlocking
                }

                val forTag = ForTag(expr.item.value, vm, mode).wrap(
                  innerTag.wrap(
                    restricted,
                    NextTag(expr.item.value).leaf
                  )
                )

                // Fix: continue execution after fold par immediately, without finding a path out from par branches
                if (innerTag == ParTag) ParTag.Detach.wrap(forTag).toFuncOp
                else forTag.toFuncOp
              }
            case _ => Raw.error("Wrong body of the `for` expression").pure[F]
          }
      )
      .namesScope(expr.token)
      .abilitiesScope(expr.token)
}

object ForSem {

  def beforeFor[S[_], F[_]: Monad](
    item: Name[S],
    iterable: ValueToken[S]
  )(using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F]
  ): F[Option[ValueRaw]] = (for {
    value <- V.valueToIterable(iterable)
    (raw, typ) = value
    _ <- OptionT.liftF(
      N.define(item, typ.element)
    )
  } yield raw).value
}
