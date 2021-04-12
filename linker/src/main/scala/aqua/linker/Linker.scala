package aqua.linker

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.monoid._

import scala.annotation.tailrec

object Linker {

  @tailrec
  def iter[I, E, T: Semigroup](
    mods: List[AquaModule[I, E, T]],
    proc: Map[I, T => T],
    cycleError: List[AquaModule[I, E, T]] => E
  ): Either[E, Map[I, T => T]] =
    mods match {
      case Nil => Right(proc)
      case _ =>
        val (canHandle, postpone) = mods.partition(_.dependsOn.keySet.forall(proc.contains))

        if (canHandle.isEmpty && postpone.nonEmpty)
          Left(cycleError(postpone))
        else
          iter(
            postpone,
            // TODO can be done in parallel
            canHandle.foldLeft(proc) { case (acc, m) =>
              val deps: T => T =
                m.dependsOn.keySet.map(acc).foldLeft[T => T](identity) { case (fAcc, f) =>
                  t => fAcc(t) |+| f(t)
                }
              acc + (m.id -> m.body.compose(deps))
            },
            cycleError
          )
    }

  def apply[I, E, T: Monoid](
    modules: Modules[I, E, T],
    cycleError: List[AquaModule[I, E, T]] => E
  ): ValidatedNec[E, Map[I, T]] =
    if (modules.dependsOn.nonEmpty) Validated.invalid(modules.dependsOn.values.reduce(_ ++ _))
    else
      Validated.fromEither(
        iter(modules.loaded.values.toList, Map.empty[I, T => T], cycleError)
          .map(_.view.filterKeys(modules.exports).mapValues(_.apply(Monoid[T].empty)).toMap)
          .left
          .map(NonEmptyChain.one)
      )

}
