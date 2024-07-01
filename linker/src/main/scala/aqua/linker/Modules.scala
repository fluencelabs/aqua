/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
    copy(loaded = loaded.view.mapValues(f).toMap)
}

object Modules {

  def from[I, E, T](modules: Chain[AquaModule[I, E, T]]): Modules[I, E, T] =
    modules.foldLeft(Modules[I, E, T]())(_.add(_, toExport = true))
}
