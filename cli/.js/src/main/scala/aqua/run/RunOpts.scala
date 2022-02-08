package aqua.run

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.types.BottomType
import aqua.{AppOpts, AquaIO, ArgOpts, FileOpts, FluenceOpts, LogFormatter}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.ArgOpts.checkDataGetServices
import aqua.builder.{ArgumentGetter, Service}
import aqua.files.AquaFilesIO
import aqua.model.transform.TransformConfig
import aqua.raw.ConstantRaw
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import cats.effect.kernel.Async
import cats.effect.{Concurrent, ExitCode, IO}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{~>, Id, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.scalajs.js
import java.util.Base64
import scala.concurrent.ExecutionContext
import scala.scalajs.js.JSON

object RunOpts extends Logging {

  val OnPeerConst = "ON_PEER"

  // Default transform config with `onPeer` constant
  def transformConfigWithOnPeer(onPeer: Option[String]) =
    TransformConfig(constants =
      onPeer.map(s => ConstantRaw(OnPeerConst, LiteralRaw.quote(s), false)).toList
    )

  /**
   * Executes a function with the specified settings
   * @param common
   *   common settings
   * @param funcName
   *   function name
   * @param inputPath
   *   path to a file with a function
   * @param imports
   *   imports that must be specified for correct compilation
   * @param args
   *   arguments to pass into a function
   * @param argumentGetters
   *   services to get argument if it is a variable
   * @param services
   *   will be registered before calling for correct execution
   * @return
   */
  def execRun[F[_]: Files: Async](
    common: GeneralRunOptions,
    funcName: String,
    inputPath: Path,
    imports: List[Path] = Nil,
    args: List[ValueRaw] = Nil,
    argumentGetters: Map[String, ArgumentGetter] = Map.empty,
    services: List[Service] = Nil
  )(implicit
    ec: ExecutionContext
  ): F[ExitCode] = {
    LogFormatter.initLogger(Some(common.logLevel))
    implicit val aio: AquaIO[F] = new AquaFilesIO[F]
    RunCommand
      .run[F](
        funcName,
        args,
        inputPath,
        imports,
        RunConfig(common, argumentGetters, services ++ builtinServices),
        transformConfigWithOnPeer(common.on)
      )
      .map(_ => ExitCode.Success)
  }

  private val builtinServices =
    aqua.builder.Console() :: aqua.builder.IPFSUploader("ipfs", "uploadFile") :: Nil

  def runOptions[F[_]: Files: AquaIO: Async](implicit
    ec: ExecutionContext
  ): Opts[F[cats.effect.ExitCode]] =
    (
      GeneralRunOptions.commonOpt,
      AppOpts.inputOpts[F],
      AppOpts.importOpts[F],
      ArgOpts.funcWithArgsOpt[F]
    ).mapN {
      case (
            common,
            inputF,
            importF,
            funcWithArgsF
          ) =>
        LogFormatter.initLogger(Some(common.logLevel))
        for {
          inputV <- inputF
          impsV <- importF
          funcWithArgsV <- funcWithArgsF
          resultV: ValidatedNec[String, F[Unit]] = inputV.andThen { input =>
            impsV.andThen { imps =>
              funcWithArgsV.andThen { funcWithArgs =>
                valid(
                  RunCommand
                    .run(
                      funcWithArgs.name,
                      funcWithArgs.args,
                      input,
                      imps,
                      RunConfig(common, funcWithArgs.getters, builtinServices),
                      transformConfigWithOnPeer(common.on)
                    )
                )
              }
            }
          }
          result <- resultV.fold(
            errs =>
              Async[F].pure {
                errs.map(logger.error)
                cats.effect.ExitCode.Error
              },
            _.map(_ => cats.effect.ExitCode.Success)
          )
        } yield {
          result
        }

    }

  def runCommand[F[_]: Files: AquaIO: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "run",
      header = "Run a function from an aqua code"
    ) {
      runOptions
    }
}
