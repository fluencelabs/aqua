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

package aqua

import aqua.io.AquaFileError

import cats.data.{Chain, EitherT, ValidatedNec}
import fs2.io.file.Path

trait AquaIO[F[_]] {
  def readFile(file: Path): EitherT[F, AquaFileError, String]

  def resolve(paths: List[Path]): EitherT[F, AquaFileError, Path]

  def listAqua(path: Path): EitherT[F, AquaFileError, Chain[Path]]

  def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit]
}

object AquaIO {
  def apply[F[_]](using aio: AquaIO[F]): AquaIO[F] = aio
}
