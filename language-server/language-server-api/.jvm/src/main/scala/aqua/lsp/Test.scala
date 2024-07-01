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

package aqua.lsp

import aqua.compiler.AquaCompilerConf
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId, Imports}
import aqua.io.AquaFileError
import aqua.lsp.LSPCompiler
import aqua.parser.lift.FileSpan
import aqua.raw.ConstantRaw
import aqua.{AquaIO, SpanParser}

import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path
import scribe.Level

object Test extends IOApp.Simple {

  given AquaIO[IO] = new AquaFilesIO[IO]

  override def run: IO[Unit] = {

    val sources = new AquaFileSources[IO](
      Path("./aqua-src/antithesis.aqua"),
      Imports(
        Map(
          Path("/") -> Imports.PathSettings(
            Map("" -> List(Path("./aqua")))
          )
        )
      )
    )
    val config = AquaCompilerConf(ConstantRaw.defaultConstants(None))

    for {
      start <- IO(System.currentTimeMillis())
      _ <- LSPCompiler
        .compileToLsp[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          config
        )
        .map {
          case Validated.Invalid(errs) =>
            errs.toChain.toList.foreach(System.err.println)
          case Validated.Valid(res) =>
            res.foreach(println)
        }
      _ <- IO.println("Compilation ends in: " + (System.currentTimeMillis() - start) + " ms")
    } yield ()
  }

}
