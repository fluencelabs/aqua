package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.parser.expr.func.ParSecExpr
import aqua.raw.value.ValueRaw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag.WaitMode
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType, StreamType}

import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ParSecSem[S[_]](val expr: ParSecExpr[S]) extends AnyVal {

  def program[F[_]: Monad](implicit
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): Prog[F, Raw] = {
    val before = (
      ForSem.beforeFor(expr.item, expr.iterable),
      OnSem.beforeOn(expr.peerId, expr.via)
    ).tupled <* A.beginScope(expr.peerId)

    val after: ((Option[ValueRaw], List[ValueRaw]), Raw) => F[Raw] =
      (pair: (Option[ValueRaw], List[ValueRaw]), ops: Raw) =>
        val (stOpt, viaVM) = pair
        A.endScope() *> (N.streamsDefinedWithinScope(), V.valueToRaw(expr.peerId)).tupled.map {
          case (streams, Some(peerId)) =>
            (stOpt, ops) match {
              case (Some(vm), FuncOp(op)) =>
                val onTag = OnTag(
                  peerId,
                  Chain.fromSeq(viaVM)
                )
                val forTag =
                  ForTag(expr.item.value, vm).wrap(
                    ParTag
                      .wrap(
                        onTag.wrap(
                          streams.toList.foldLeft(op) { case (tree, (streamName, streamType)) =>
                            RestrictionTag(streamName, streamType).wrap(tree)
                          }
                        ),
                        NextTag(expr.item.value).leaf
                      )
                  )

                forTag.toFuncOp
              case _ =>
                Raw.error("Wrong body of the `parsec` expression")
            }
          case _ =>
            Raw.error("ParSecSem: Impossible error")
        }

    Prog
      .around(
        before,
        after
      )
      .namesScope[S](expr.token)
      .abilitiesScope[S](expr.token)
  }
}
