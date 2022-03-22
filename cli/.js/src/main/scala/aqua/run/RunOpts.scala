package aqua.run

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{LiteralToken, VarToken}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.types.BottomType
import aqua.{
  AppOpts,
  AquaIO,
  ArgOpts,
  CommandBuilder,
  FileOpts,
  FluenceOpts,
  FuncWithData,
  LogFormatter,
  RelativePath,
  RunInfo,
  SubCommandBuilder
}
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

  def runOptsCompose[F[_]: Files: Concurrent]
    : Opts[F[ValidatedNec[String, (Path, List[Path], FuncWithData)]]] = {
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], ArgOpts.funcWithArgsOpt[F]).mapN {
      case (inputF, importF, funcWithArgsF) =>
        for {
          inputV <- inputF
          importV <- importF
          funcWithArgsV <- funcWithArgsF
        } yield {
          (inputV, importV, funcWithArgsV).mapN { case (i, im, f) =>
            (i, im, f)
          }
        }
    }
  }

  def runOptions[F[_]: AquaIO: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.applyF(
      name = "run",
      header = "Run Aqua code",
      (
        GeneralRunOptions.commonOpt,
        runOptsCompose[F]
      ).mapN {
        case (
              common,
              optionsF
            ) =>
          LogFormatter.initLogger(Some(common.logLevel))
          optionsF.map(
            _.map { case (input, imps, funcWithArgs) =>
              RunInfo(
                common,
                funcWithArgs.func,
                RelativePath(input),
                imps,
                funcWithArgs.getters
              )
            }
          )
      }
    )

  def runCommand[F[_]: Files: AquaIO: Async]: Command[F[ExitCode]] =
    runOptions.command
}
