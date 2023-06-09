package aqua

import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.logging.LogFormatter
import aqua.model.transform.TransformConfig
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path
import scribe.Level

object Test extends IOApp.Simple {

  implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

  override def run: IO[Unit] = {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(formatter = LogFormatter.formatterWithFilename, minimumLevel = Some(Level.Info))
      .replace()
    for {
      start <- IO(System.currentTimeMillis())
      _ <- AquaPathCompiler
        .compileFilesTo[IO](
          Path("./aqua-src/antithesis.aqua"),
          List(
            Path("./aqua-src/testImport1"),
            Path("./aqua-src/testImport2"),
            Path("./aqua-src/testImport1/insideLib1"),
            Path("./aqua-src/testImport2/insideLib2")
          ),
          Option(Path("./target")),
          TypeScriptBackend(false, "IFluenceClient$$"),
          TransformConfig(wrapWithXor = false),
          false
        )
        .map {
          case Validated.Invalid(errs) =>
            errs.map(System.err.println): Unit
          case Validated.Valid(res) =>
            res.map(println): Unit
        }
      _ <- IO.println("Compilation ends in: " + (System.currentTimeMillis() - start) + " ms")
    } yield ()
  }

}
