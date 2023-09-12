package aqua.api

import aqua.api.TargetType.{JavaScriptType, TypeScriptType}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiled
import aqua.files.FileModuleId
import cats.data.Chain
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import cats.data.Validated.{Valid, Invalid}
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
        case Valid(res: Chain[AquaCompiled[FileModuleId]]) =>
          val content = res.get(0).get.compiled.head.content

          val targetPath =  Path("./target/antithesis.ts")
          Stream.emit(content)
            .through(text.utf8.encode)
            .through(Files[IO].writeAll(targetPath))
            .attempt

            .compile
            .last.map(_ => println(s"File: ${targetPath.absolute.normalize}"))
        case Invalid(e) =>
          IO.pure(println(e))

      }
  }
}
