package aqua.linker

import cats.data.NonEmptyChain
import cats.syntax.option._

case class Modules[I, E, T](
  loaded: Map[I, AquaModule[I, E, T]] = Map.empty[I, AquaModule[I, E, T]],
  dependsOn: Map[I, NonEmptyChain[E]] = Map.empty[I, NonEmptyChain[E]],
  exports: Set[I] = Set.empty[I]
) {

  def add(aquaModule: AquaModule[I, E, T], export: Boolean = false): Modules[I, E, T] =
    if (loaded.contains(aquaModule.id)) this
    else
      copy(
        loaded = loaded + (aquaModule.id -> aquaModule),
        dependsOn = aquaModule.dependsOn.foldLeft(dependsOn - aquaModule.id) {
          case (deps, (moduleId, _)) if loaded.contains(moduleId) || moduleId == aquaModule.id =>
            deps
          case (deps, (moduleId, err)) =>
            deps.updatedWith(moduleId)(_.fold(NonEmptyChain.one(err))(_.append(err)).some)
        },
        exports = if (export) exports + aquaModule.id else exports
      )

  def isResolved: Boolean = dependsOn.isEmpty
}
