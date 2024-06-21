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

package aqua.api

import aqua.api.TargetType.TypeScriptType
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiled
import aqua.files.FileModuleId

import cats.data.Chain
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Test extends IOApp.Simple {

  override def run: IO[Unit] = {

    APICompilation
      .compilePath(
        "./aqua-src/antithesis.aqua",
        Imports.fromMap(Map("/" -> Map("" -> List("./aqua")))),
        AquaAPIConfig(targetType = TypeScriptType, noXor = true),
        TypeScriptBackend(false, "IFluenceClient$$")
      )
      .timed
      .flatMap { case (duration, res) =>
        println("Compilation time: " + duration.toMillis)
        val (warnings, result) = res.value.run

        IO.delay {
          warnings.toList.foreach(println)
        } *> result.fold(
          errors =>
            IO.delay {
              errors.toChain.toList.foreach(println)
            },
          compiled => {
            val content = compiled.get(0).get.compiled.head.content
            val targetPath = Path("./target/antithesis.ts")

            Stream
              .emit(content)
              .through(text.utf8.encode)
              .through(Files[IO].writeAll(targetPath))
              .attempt
              .compile
              .last *> IO.delay(
              println(s"File: ${targetPath.absolute.normalize}")
            )
          }
        )

      }
  }
}
