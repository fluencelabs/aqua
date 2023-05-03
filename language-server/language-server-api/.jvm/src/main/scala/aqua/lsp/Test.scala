package aqua.lsp

import aqua.compiler.AquaCompilerConf
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.lsp.LSPCompiler
import aqua.parser.lift.FileSpan
import aqua.{AquaIO, SpanParser}
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path
import scribe.Level

object Test extends IOApp.Simple {

  implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

  override def run: IO[Unit] = {

    val sources = new AquaFileSources[IO](Path("./aqua-src/antithesis.aqua"), List(Path("./aqua")))
    val config = AquaCompilerConf()

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
            errs.map(System.err.println): Unit
          case Validated.Valid(res) =>
            res.map(println): Unit
        }
      _ <- IO.println("Compilation ends in: " + (System.currentTimeMillis() - start) + " ms")
    } yield ()
  }

}
