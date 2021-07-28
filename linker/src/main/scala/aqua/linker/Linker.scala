package aqua.linker

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.monoid._
import wvlet.log.LogSupport

import scala.annotation.tailrec

object Linker extends LogSupport {

  @tailrec
  def iter[I, E, T: Semigroup](
    mods: List[AquaModule[I, E, T => T]],
    proc: Map[I, T => T],
    cycleError: List[AquaModule[I, E, T => T]] => E
  ): Either[E, Map[I, T => T]] =
    mods match {
      case Nil =>
        Right(proc)
      case _ =>
        val (canHandle, postpone) = mods.partition(_.dependsOn.keySet.forall(proc.contains))
        debug("ITERATE, can handle: " + canHandle.map(_.id))
        debug(s"proc = ${proc.keySet}")

        if (canHandle.isEmpty && postpone.nonEmpty)
          Left(cycleError(postpone))
        else {
          val folded = canHandle.foldLeft(proc) { case (acc, m) =>
            val importKeys = m.dependsOn.keySet
            debug(m.id + " dependsOn " + importKeys)
            val deps: T => T =
              importKeys.map(acc).foldLeft[T => T](identity) { case (fAcc, f) =>
                debug("COMBINING ONE TIME ")
                t => {
                  debug(s"call combine $t")
                  fAcc(t) |+| f(t)
                }
              }
            acc + (m.id -> m.body.compose(deps))
          }
          iter(
            postpone,
            // TODO can be done in parallel
            folded,
            cycleError
          )
        }
    }

  def link[I, E, T: Monoid](
    modules: Modules[I, E, T => T],
    cycleError: List[AquaModule[I, E, T => T]] => E
  ): ValidatedNec[E, Map[I, T]] =
    if (modules.dependsOn.nonEmpty) Validated.invalid(modules.dependsOn.values.reduce(_ ++ _))
    else {
      val result = iter(modules.loaded.values.toList, Map.empty[I, T => T], cycleError)

      Validated.fromEither(
        result
          .map(_.view.filterKeys(modules.exports).mapValues(_.apply(Monoid[T].empty)).toMap)
          .left
          .map(NonEmptyChain.one)
      )
    }

}
