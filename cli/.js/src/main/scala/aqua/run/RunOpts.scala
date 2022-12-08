package aqua.run

import aqua.*
import aqua.ArgOpts.checkDataGetServices
import aqua.builder.{ArgumentGetter, Service}
import aqua.model.transform.TransformConfig
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{LiteralToken, VarToken}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.logging.LogFormatter
import aqua.raw.ConstantRaw
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.run.plugin.Plugin
import aqua.types.BottomType
import cats.data.*
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.effect.kernel.Async
import cats.effect.{Concurrent, ExitCode, IO}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Id, Monad, ~>}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import java.util.Base64
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSON

object RunOpts extends Logging {

  val OnPeerConst = "ON_PEER"

  // Default transform config with `onPeer` constant
  def transformConfig(
    onPeer: Option[String],
    constants: List[ConstantRaw],
    noXor: Boolean,
    noRelay: Boolean
  ): TransformConfig = {
    val tc = TransformConfig(
      constants =
        onPeer.map(s => ConstantRaw(OnPeerConst, LiteralRaw.quote(s), false)).toList ++ constants,
      wrapWithXor = !noXor
    )
    tc.copy(relayVarName = tc.relayVarName.filterNot(_ => noRelay))
  }

  def runOptsCompose[F[_]: Files: Concurrent]
    : Opts[F[ValidatedNec[String, (Option[AquaPath], List[Path], FuncWithData, Option[NonEmptyList[JsonService]], List[String])]]] = {
    (
      AppOpts.wrapWithOption(AppOpts.inputOpts[F]),
      AppOpts.importOpts[F],
      ArgOpts.funcWithArgsOpt[F],
      AppOpts.wrapWithOption(JsonService.jsonServiceOpt),
      AppOpts.wrapWithOption(Plugin.opt)
    ).mapN { case (inputF, importF, funcWithArgsF, jsonServiceOp, pluginsOp) =>
      for {
        inputV: ValidatedNec[String, Option[AquaPath]] <-
          inputF.map(_.map(_.map(p => Option(RelativePath(p))))).getOrElse {
            validNec[String, Option[AquaPath]](None).pure[F]
          }
        importV <- importF
        funcWithArgsV <- funcWithArgsF
        jsonServiceV <- jsonServiceOp
          .map(_.map(_.map(js => Some(js))))
          .getOrElse(validNec[String, Option[NonEmptyList[JsonService]]](None).pure[F])
        pluginsPathsV <- pluginsOp.getOrElse(validNec[String, List[String]](Nil).pure[F])
      } yield {
        (inputV, importV, funcWithArgsV, jsonServiceV, pluginsPathsV).mapN { case (i, im, f, j, p) =>
          (i, im, f, j, p)
        }
      }
    }
  }

  def runOptions[F[_]: AquaIO: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.applyF(
      name = "run",
      header = "Run Aqua code",
      (
        GeneralOptions.runOpt,
        runOptsCompose[F]
      ).mapN {
        case (
              common,
              optionsF
            ) =>
          LogFormatter.initLogger(Some(common.logLevel.compiler))
          optionsF.map(
            _.map { case (input, imps, funcWithArgs, services, pluginsPaths) =>
              RunInfo(
                common,
                funcWithArgs.func,
                input,
                imps,
                funcWithArgs.getters,
                Nil,
                services.map(_.toList).getOrElse(Nil),
                pluginsPaths
              )
            }
          )
      }
    )

  def runCommand[F[_]: Files: AquaIO: Async]: Command[F[ValidatedNec[String, Unit]]] =
    runOptions.command
}
