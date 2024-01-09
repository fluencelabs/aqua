package aqua.linker

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.syntax.functor.*
import cats.instances.list.*
import scribe.Logging

import scala.annotation.tailrec

object Linker extends Logging {

  // Dependency Cycle, prev element import next
  // and last imports head
  type DepCycle[I] = NonEmptyChain[I]

  /**
   * Find dependecy cycles in modules
   *
   * @param mods Modules
   * @return [[List]] of dependecy cycles found
   */
  private def findDepCycles[I, E, T](
    mods: List[AquaModule[I, E, T => T]]
  ): List[DepCycle[AquaModule[I, E, T => T]]] = {
    val modsIds = mods.map(_.id).toSet
    // Limit search to only passed modules (there maybe dependencies not from `mods`)
    val deps = mods.map(m => m.id -> m.dependsOn.keySet.intersect(modsIds)).toMap

    // DFS traversal of dependency graph
    @tailrec
    def findCycles(
      paths: List[NonEmptyChain[I]],
      visited: Set[I],
      result: List[DepCycle[I]]
    ): List[DepCycle[I]] = paths match {
      case Nil => result
      case path :: otherPaths =>
        val pathDeps = deps.get(path.last).toList.flatten
        val cycles = pathDeps.flatMap(dep =>
          NonEmptyChain.fromChain(
            // This is slow
            path.toChain.dropWhile(_ != dep)
          )
        )
        val newPaths = pathDeps
          .filterNot(visited.contains)
          .map(path :+ _) ++ otherPaths

        findCycles(
          paths = newPaths,
          visited = visited ++ pathDeps,
          result = cycles ++ result
        )
    }

    val cycles = mods
      .flatMap(m =>
        findCycles(
          paths = NonEmptyChain.one(m.id) :: Nil,
          visited = Set(m.id),
          result = List.empty
        )
      )
      .distinctBy(
        // This is really slow, but there
        // should not be a lot of cycles
        _.toChain.toList.toSet
      )

    val modsById = mods.fproductLeft(_.id).toMap

    // This should be safe
    cycles.map(_.map(modsById))
  }

  @tailrec
  def iter[I, E, T: Semigroup](
    mods: List[AquaModule[I, E, T => T]],
    proc: Map[I, T => T],
    cycleError: DepCycle[AquaModule[I, E, T => T]] => E
  ): ValidatedNec[E, Map[I, T => T]] =
    mods match {
      case Nil =>
        proc.valid
      case _ =>
        val (canHandle, postpone) = mods.partition(_.dependsOn.keySet.forall(proc.contains))
        logger.debug("ITERATE, can handle: " + canHandle.map(_.id))
        logger.debug(s"dependsOn = ${mods.map(_.dependsOn.keySet)}")
        logger.debug(s"postpone = ${postpone.map(_.id)}")
        logger.debug(s"proc = ${proc.keySet}")

        if (canHandle.isEmpty && postpone.nonEmpty) {
          findDepCycles(postpone)
            .map(cycleError)
            .invalid
            .leftMap(
              // This should be safe as cycles should exist at this moment
              errs => NonEmptyChain.fromSeq(errs).get
            )
        } else {
          val folded = canHandle.foldLeft(proc) { case (acc, m) =>
            val importKeys = m.dependsOn.keySet
            logger.debug(s"${m.id} dependsOn $importKeys")
            val deps: T => T =
              importKeys.map(acc).foldLeft(identity[T]) { case (fAcc, f) =>
//                logger.debug("COMBINING ONE TIME ")
                t => {
//                  logger.debug(s"call combine $t")
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

  def link[I, E, T: Semigroup](
    modules: Modules[I, E, T => T],
    cycleError: DepCycle[AquaModule[I, E, T => T]] => E,
    empty: I => T
  ): ValidatedNec[E, Map[I, T]] =
    if (modules.dependsOn.nonEmpty) Validated.invalid(modules.dependsOn.values.reduce(_ ++ _))
    else {
      val result = iter(modules.loaded.values.toList, Map.empty, cycleError)

      result.map(_.collect {
        case (i, f) if modules.exports(i) =>
          i -> f(empty(i))
      })
    }

}
