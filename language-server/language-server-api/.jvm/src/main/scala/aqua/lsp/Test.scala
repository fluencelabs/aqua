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
