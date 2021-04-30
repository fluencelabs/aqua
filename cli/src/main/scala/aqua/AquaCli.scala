package aqua

import aqua.model.transform.BodyConfig
import cats.data.Validated
import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import cats.syntax.apply._

import java.nio.file.Path

object AquaCli extends IOApp {

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

  val noRelay: Opts[Boolean] =
    Opts
      .flag("no-relay", "Do not generate a pass through the relay node")
      .map(_ => true)
      .withDefault(false)

  val noXorWrapper: Opts[Boolean] =
    Opts
      .flag("no-xor", "Do not generate a wrapper that catches and displays errors")
      .map(_ => true)
      .withDefault(false)

  def mainOpts: Opts[IO[ExitCode]] =
    (inputOpts, importOpts, outputOpts, compileToAir, noRelay, noXorWrapper).mapN {
      case (input, imports, output, toAir, noRelay, noXor) =>
        AquaCompiler
          .compileFilesTo[IO](
            input,
            imports,
            output,
            if (toAir) AquaCompiler.AirTarget else AquaCompiler.TypescriptTarget, {
              val bc = BodyConfig(wrapWithXor = !noXor)
              bc.copy(relayVarName = bc.relayVarName.filterNot(_ => noRelay))
            }
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

  override def run(args: List[String]): IO[ExitCode] =
    CommandIOApp.run[IO](
      "aqua-c",
      "Aquamarine compiler",
      helpFlag = true,
      Option(getClass.getPackage.getImplementationVersion).filter(_.nonEmpty)
    )(mainOpts, args)
}
