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

package aqua.files

import aqua.AquaIO
import aqua.helpers.ext.Extension
import aqua.io.*
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.effect.kernel.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import fs2.text

import scala.util.Try

class AquaFilesIO[F[_]: Files: Concurrent] extends AquaIO[F] {

  override def readFile(file: Path): EitherT[F, AquaFileError, String] =
    EitherT(
      Files[F]
        .readUtf8(file)
        .foldMonoid
        // TODO fix for comment on last line in air
        // TODO should be fixed by parser
        .map(_.appendedAll("\n\r"))
        .attempt
        .map(_.leftMap(FileSystemError.apply))
        .compile
        .last
        .map(_.getOrElse(EmptyFileError(file).asLeft))
    )

  /**
   * Return first path that is a regular file
   */
  override def resolve(
    paths: List[Path]
  ): EitherT[F, AquaFileError, Path] =
    paths
      .collectFirstSomeM(p =>
        Concurrent[F]
          .attemptT(Files[F].isRegularFile(p))
          .recover(_ => false)
          .leftMap(FileSystemError.apply)
          .map(Option.when(_)(p))
      )
      .flatMap {
        case None =>
          EitherT.leftT(
            FilesUnresolved(paths)
          )
        case Some(p) =>
          Try(
            p.absolute.normalize
          ).toEither.leftMap(FileSystemError.apply).toEitherT
      }

  // Get all `.aqua` files inside if the path is a directory or
  // this path if it is an `.aqua` file otherwise
  override def listAqua(path: Path): EitherT[F, AquaFileError, Chain[Path]] =
    for {
      exists <- EitherT.liftF(Files[F].exists(path))
      _ <- EitherT.cond(exists, (), FileNotFound(path): AquaFileError)
      paths <- EitherT.liftF(
        Files[F]
          .walk(path)
          .evalFilter(p =>
            Files[F]
              .isRegularFile(p)
              .map(_ && p.extName == Extension.aqua)
          )
          .compile
          .toList
      )
    } yield Chain.fromSeq(paths)

  private def deleteIfExists(file: Path): EitherT[F, AquaFileError, Boolean] =
    Files[F].deleteIfExists(file).attemptT.leftMap(FileSystemError.apply)

  private def createDirectories(path: Path): EitherT[F, AquaFileError, Unit] =
    Files[F].createDirectories(path).attemptT.leftMap(FileSystemError.apply(_): AquaFileError)

  // Writes to a file, creates directories if they do not exist
  override def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit] =
    deleteIfExists(file) >> file.parent
      .map(createDirectories)
      .getOrElse(EitherT.liftF(().pure[F])) >>
      EitherT(
        fs2.Stream
          .emit(content)
          .through(text.utf8.encode)
          .through(Files[F].writeAll(file))
          .attempt
          .compile
          .last
          .map(_.getOrElse(Right(())))
      )
        .leftMap(FileWriteError(file, _))

}
