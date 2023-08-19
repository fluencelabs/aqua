package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.parser.expr.func.ParSeqExpr
import aqua.raw.value.ValueRaw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType, StreamType}

import cats.Monad
import cats.data.Chain
import cats.syntax.option.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ParSeqSem[S[_]](val expr: ParSeqExpr[S]) extends AnyVal {

  def program[F[_]: Monad](implicit
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): Prog[F, Raw] =
    Prog
      .around(
        (
          ForSem.beforeFor(expr.item, expr.iterable),
          OnSem.beforeOn(expr.peerId, expr.via)
        ).tupled,
        // Without type of ops specified
        // scala compiler fails to compile this
        (iterableVia, ops: Raw) => {
          val (iterableVM, viaVM) = iterableVia
          after(iterableVM, viaVM, ops)
        }
      )
      .namesScope(expr.token)
      .abilitiesScope(expr.token)

  private def after[F[_]: Monad](
    iterableVM: Option[ValueRaw],
    viaVM: List[ValueRaw],
    ops: Raw
  )(using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): F[Raw] =
    V.valueToRaw(expr.peerId).map((_, iterableVM, ops)).flatMap {
      case (Some(peerId), Some(vm), FuncOp(op)) =>
        for {
          restricted <- FuncOpSem.restrictStreamsInScope(op)
          onTag = OnTag(
            peerId = peerId,
            via = Chain.fromSeq(viaVM),
            strategy = OnTag.ReturnStrategy.Relay.some
          )
          tag = ForTag(expr.item.value, vm).wrap(
            ParTag.wrap(
              onTag.wrap(restricted),
              NextTag(expr.item.value).leaf
            )
          )
        } yield tag.toFuncOp
      case (None, _, _) => Raw.error("ParSecSem: could not resolve `peerId`").pure
      case (_, None, _) => Raw.error("ParSecSem: could not resolve `iterable`").pure
      case (_, _, _) => Raw.error("ParSecSem: wrong body of `parsec` block").pure
    }
}
