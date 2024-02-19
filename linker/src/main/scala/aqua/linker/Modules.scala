package aqua.linker

import cats.Foldable
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.foldable._
import cats.syntax.option._

case class Modules[I, E, T](
  loaded: Map[I, AquaModule[I, E, T]] = Map.empty[I, AquaModule[I, E, T]],
  dependsOn: Map[I, NonEmptyChain[E]] = Map.empty[I, NonEmptyChain[E]],
  exports: Set[I] = Set.empty[I]
) {

  def add(aquaModule: AquaModule[I, E, T], toExport: Boolean = false): Modules[I, E, T] =
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
        exports = if (toExport) exports + aquaModule.id else exports
      )

  def addAll[F[_]: Foldable](modules: F[AquaModule[I, E, T]]): Modules[I, E, T] =
    modules.foldLeft(this)(_ add _)

  def isResolved: Boolean = dependsOn.isEmpty

  def map[TT](f: AquaModule[I, E, T] => AquaModule[I, E, TT]): Modules[I, E, TT] =
    copy(loaded = loaded.view.mapValues(_.map(f)).toMap)

  def mapErr[EE](f: E => EE): Modules[I, EE, T] =
    copy(
      loaded = loaded.view.mapValues(_.mapErr(f)).toMap,
      dependsOn = dependsOn.view.mapValues(_.map(f)).toMap
    )
}

object Modules {

  def from[I, E, T](modules: Chain[AquaModule[I, E, T]]): Modules[I, E, T] =
    modules.foldLeft(Modules[I, E, T]())(_.add(_, toExport = true))
}
