package aqua.linker

import cats.data.NonEmptyChain
import cats.syntax.option._

case class Modules[I, E, T](
  loaded: Map[I, AquaModule[I, E, T]] = Map.empty[I, AquaModule[I, E, T]],
  dependsOn: Map[I, NonEmptyChain[E]] = Map.empty[I, NonEmptyChain[E]],
  exports: Set[I] = Set.empty[I]
) {

  def add(m: AquaModule[I, E, T], export: Boolean = false): Modules[I, E, T] =
    if (loaded.contains(m.id)) this
    else
      copy(
        loaded = loaded + (m.id -> m),
        dependsOn = m.dependsOn.foldLeft(dependsOn - m.id) {
          case (deps, (mId, _)) if loaded.contains(mId) || mId == m.id => deps
          case (deps, (mId, err)) =>
            deps.updatedWith(mId)(_.fold(NonEmptyChain.one(err))(_.append(err)).some)
        },
        exports = if (export) exports + m.id else exports
      )

  def isResolved: Boolean = dependsOn.isEmpty
}
