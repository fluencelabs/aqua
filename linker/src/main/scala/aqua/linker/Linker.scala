package aqua.linker

import aqua.errors.Errors.internalError

import cats.MonadError
import cats.data.NonEmptyChain
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import scala.annotation.tailrec
import scribe.Logging

object Linker extends Logging {

  // Transpilation function for module
  // (Imports contexts => Compilation result)
  type TP = [F[_], T] =>> Map[String, T] => F[T]

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
    mods: List[AquaModule[I, E, T]]
  ): List[DepCycle[I]] = {
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

    mods
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
  }

  /**
   * Main iterative linking function
   * @param mods Modules to link
   * @param proc Already processed modules
   * @param cycle Function to create error from dependency cycle
   * @return Result for all modules
   */
  def iter[I, E, F[_], T](
    mods: List[AquaModule[I, E, TP[F, T]]],
    proc: Map[I, T],
    cycle: DepCycle[I] => E
  )(using me: MonadError[F, NonEmptyChain[E]]): F[Map[I, T]] =
    mods match {
      case Nil =>
        proc.pure
      case _ =>
        // Find modules that can be processed
        val (canHandle, postpone) = mods.partition(
          _.dependsOn.keySet.forall(proc.contains)
        )
        logger.debug("ITERATE, can handle: " + canHandle.map(_.id))
        logger.debug(s"dependsOn = ${mods.map(_.dependsOn.keySet)}")
        logger.debug(s"postpone = ${postpone.map(_.id)}")
        logger.debug(s"proc = ${proc.keySet}")

        // If there are no modules that can be processed
        if (canHandle.isEmpty && postpone.nonEmpty) {
          me.raiseError(
            // This should be safe as cycles should exist at this moment
            NonEmptyChain
              .fromSeq(findDepCycles(postpone).map(cycle))
              .get
          )
        } else
          canHandle.traverse { mod =>
            // Gather all imports for module
            val imports = mod.imports.view.mapValues { imp =>
              proc
                .getOrElse(
                  imp,
                  // Should not happen as we check it above
                  internalError(s"Module $imp not found in $proc")
                )
            }.toMap

            // Process (transpile) module
            mod.body(imports).map(mod.id -> _)
          }.flatMap(processed =>
            // flatMap should be stack safe
            iter(
              postpone,
              proc ++ processed,
              cycle
            )
          )
    }

  /**
   * Link modules
   *
   * @param modules Modules to link (with transpilation functions as bodies)
   * @param cycle Function to create error from dependency cycle
   * @return Result for all **exported** modules
   */
  def link[I, E, F[_], T](
    modules: Modules[I, E, TP[F, T]],
    cycle: DepCycle[I] => E
  )(using me: MonadError[F, NonEmptyChain[E]]): F[Map[I, T]] =
    if (modules.dependsOn.nonEmpty)
      me.raiseError(
        modules.dependsOn.values.reduce(_ ++ _)
      )
    else
      iter(modules.loaded.values.toList, Map.empty, cycle).map(
        // Remove all modules that are not exported from result
        _.view.filterKeys(modules.exports.contains).toMap
      )
}
