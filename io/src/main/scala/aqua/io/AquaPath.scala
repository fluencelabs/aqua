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

package aqua.io

import aqua.PlatformPackagePath

import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import fs2.io.file.Path

sealed trait AquaPath {
  def getPath[F[_]: Async](): F[Path]
}

// Path for package relative files
case class PackagePath(path: String) extends AquaPath {
  def getPath[F[_]: Async](): F[Path] = PlatformPackagePath.getPackagePath(path)
}

// Path for absolute or call path relative files
case class RelativePath(path: Path) extends AquaPath {
  def getPath[F[_]: Async](): F[Path] = path.pure[F]
}

object PackagePath {
  // path to a builtin file in aqua package
  val builtin: PackagePath = PackagePath("../aqua-lib/builtin.aqua")
}
