package aqua

import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.model.transform.TransformConfig
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path

object Test extends IOApp.Simple {

  implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

  override def run: IO[Unit] =
    IO.println("Start ms: " + System.currentTimeMillis()) *>
      AquaPathCompiler
        .compileFilesTo[IO](
          Path("./aqua-src/import.aqua"),
          List(Path("./aqua")),
          Path("./target"),
          TypeScriptBackend,
          TransformConfig()
        )
        .map {
          case Validated.Invalid(errs) =>
            errs.map(System.err.println): Unit
          case Validated.Valid(res) =>
            res.map(println): Unit
        } <* IO.println("End ms  : " + System.currentTimeMillis())

}
