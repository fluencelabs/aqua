package aqua

import cats.data.Validated
import cats.effect.{ExitCode, IO}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import cats.syntax.apply._

import java.nio.file.Path

object AquaCli
    extends CommandIOApp(
      name = "aqua-c",
      header = "Aquamarine compiler",
      // TODO get version from SBT!
      version = "0.1.0"
    ) {

  val inputOpts: Opts[Path] =
    Opts.option[Path]("input", "Path to the input directory that contains your .aqua files", "i")

  val outputOpts: Opts[Path] =
    Opts.option[Path]("output", "Path to the output directory", "o")

  val importOpts: Opts[LazyList[Path]] =
    Opts
      .options[Path]("import", "Path to the directory to import from", "m")
      .map(_.toList.to(LazyList))
      .withDefault(LazyList.empty)

  val compileToAir: Opts[Boolean] =
    Opts
      .flag("air", "Generate .air file instead of typescript", "a")
      .map(_ => true)
      .withDefault(false)

  override def main: Opts[IO[ExitCode]] =
    (inputOpts, importOpts, outputOpts, compileToAir).mapN { case (input, imports, output, toAir) =>
      AquaCompiler
        .compileFilesTo[IO](
          input,
          imports,
          output,
          if (toAir) AquaCompiler.AirTarget else AquaCompiler.TypescriptTarget
        )
        .map {
          case Validated.Invalid(errs) =>
            errs.map(println)
            ExitCode.Error
          case Validated.Valid(res) =>
            res.map(println)
            ExitCode.Success
        }
    }
}
