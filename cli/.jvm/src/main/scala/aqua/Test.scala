package aqua

import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.model.transform.GenerationConfig
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path

object Test extends IOApp.Simple {

  implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

  override def run: IO[Unit] =
    AquaPathCompiler
      .compileFilesTo[IO](
        Path("./aqua-src"),
        List(Path("./aqua")),
        Path("./target"),
        TypeScriptBackend,
        GenerationConfig()
      )
      .map {
        case Validated.Invalid(errs) =>
          errs.map(System.err.println): Unit
        case Validated.Valid(res) =>
          res.map(println): Unit
      }

}
