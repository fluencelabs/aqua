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
import aqua.compiler.{AquaCompiled, AquaSources}
import aqua.io.FilesUnresolved
import aqua.io.{AquaFileError, FileSystemError, ListAquaErrors}
import aqua.syntax.eithert.*

import cats.data.EitherT
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Functor, Monad}
import fs2.io.file.{Files, Path}
import scala.util.Try
import scribe.Logging

trait AquaFileImports[F[_]: Functor: AquaIO] extends AquaSources[F, AquaFileError, FileModuleId] {
  def imports: Imports

  override def resolveImport(
    from: FileModuleId,
    imported: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] =
    AquaIO[F]
      .resolve(
        imports.resolutions(
          // NOTE: It is important to use normalized absolute path here
          from.file.normalize.absolute,
          imported
        )
      )
      .leftMap {
        case e: FilesUnresolved =>
          e.toImportUnresolved(imported)
        case e => e
      }
      .map(FileModuleId.apply)
      .toValidatedNec

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] =
    AquaIO[F].readFile(file.file).toValidatedNec
}

/**
 * Aqua sources that are read from file system.
 */
class AquaFileSources[F[_]: Monad: AquaIO](
  sourcesPath: Path,
  override val imports: Imports
) extends AquaFileImports[F] with Logging {

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    (for {
      files <- AquaIO[F]
        .listAqua(sourcesPath)
        .transform(_.toEitherNec)
      contents <- EitherT.fromValidatedF(
        files
          .traverse(file =>
            AquaIO[F]
              .readFile(file)
              .map(content => FileModuleId(file) -> content)
              .toValidatedNec
          )
          .map(_.sequence)
      )
    } yield contents).toValidated
}

/**
 * Aqua sources that are read from string map.
 */
class AquaStringSources[F[_]: Monad: AquaIO](
  sourcesMap: Map[FileModuleId, String],
  override val imports: Imports
) extends AquaFileImports[F] {

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    Chain.fromSeq(sourcesMap.toSeq).validNec.pure[F]
}
