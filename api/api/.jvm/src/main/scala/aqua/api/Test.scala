package aqua.api

import aqua.api.TargetType.TypeScriptType
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiled
import aqua.files.FileModuleId

import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Test extends IOApp.Simple {

  override def run: IO[Unit] = {
    APICompilation
      .compilePath(
        "./aqua-src/antithesis.aqua",
        "./aqua" :: Nil,
        AquaAPIConfig(targetType = TypeScriptType),
        TypeScriptBackend(false, "IFluenceClient$$")
      )
      .flatMap {
        case Valid(res) =>
          val content = res.get(0).get.compiled.head.content
          val targetPath =  Path("./target/antithesis.ts")

          Stream.emit(content)
            .through(text.utf8.encode)
            .through(Files[IO].writeAll(targetPath))
            .attempt
            .compile
            .last.flatMap(_ => IO.delay(println(s"File: ${targetPath.absolute.normalize}")))
        case Invalid(e) =>
          IO.delay(println(e))

      }
  }
}
