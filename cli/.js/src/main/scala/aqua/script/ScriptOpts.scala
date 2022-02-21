package aqua.script

import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.{AquaFileError, OutputPrinter}
import aqua.ipfs.js.IpfsApi
import aqua.js.{Config, Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import aqua.model.{AquaContext, FuncArrow, LiteralModel}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.run.RunCommand.createKeyPair
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.*
import aqua.run.RunOpts.logger
import aqua.types.{ArrowType, LiteralType, NilType, ScalarType}
import aqua.ArgOpts.{funcWithArgsOpt, funcWithLiteralsOpt}
import aqua.backend.Generated
import aqua.backend.air.{AirBackend, AirGen, FuncAirGen}
import aqua.builder.ArgumentGetter
import aqua.compiler.AquaCompiler
import aqua.model.transform.{Transform, TransformConfig}
import aqua.parser.lift.FileSpan
import aqua.raw.ops.{Call, CallArrowTag}
import aqua.res.{AquaRes, FuncRes}
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.ExecutionContext

object ScriptOpts extends Logging {

  val ScriptAqua = "aqua/script.aqua"

  val AddFuncName = "schedule"
  val RemoveFuncName = "remove"
  val ListFuncName = "list"

  def scriptOpt[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "script",
      header = "Manage scheduled scripts"
    ) {
      Opts.subcommand(schedule) orElse
        Opts.subcommand(remove) orElse
        Opts.subcommand(list)
    }

  def intervalOpt: Opts[Option[Int]] =
    AppOpts.wrapWithOption(
      Opts
        .option[Int]("interval", "Indicating how often the script will run in seconds", "n")
    )

  def scriptIdOpt: Opts[String] =
    Opts
      .option[String]("script-id", "Script id to remove", "i")

  private def findFunction(contexts: Chain[AquaContext], funcName: String): Option[FuncArrow] =
    contexts
      .collectFirstSome(_.allFuncs.get(funcName))

  def generateAir(callable: FuncArrow, transformConfig: TransformConfig): String = {
    val funcRes = Transform.funcRes(callable, transformConfig).value
    AirGen(funcRes.body).generate.show
  }

  def schedule[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "add",
      header = "Upload aqua function as a scheduled script."
    ) {
      (
        GeneralRunOptions.commonOptWithSecretKey,
        scheduleOptsCompose[F],
        intervalOpt
      ).mapN { (common, optionsF, intervalOp) =>
        LogFormatter.initLogger(Some(common.logLevel))
        optionsF.flatMap(
          _.map { case (input, imports, funcWithArgs, aquaPath) =>
            implicit val aio: AquaIO[F] = new AquaFilesIO[F]
            val tConfig = TransformConfig(relayVarName = None, wrapWithXor = false)
            val funcCompiler =
              new FuncCompiler[F](
                input,
                imports,
                tConfig,
                withRunImport = true
              )

            val intervalArg =
              intervalOp
                .map(i => LiteralRaw(i.toString, LiteralType.number))
                .getOrElse(ValueRaw.Nil)

            for {
              callableV <- funcCompiler.compile(funcWithArgs.name)
              wrappedBody = CallArrowTag(funcWithArgs.name, Call(funcWithArgs.args, Nil)).leaf
              result: ValidatedNec[String, ExitCode] <- callableV
                .map(callable =>
                  generateAir(
                    FuncArrow(
                      funcWithArgs.name + "_scheduled",
                      wrappedBody,
                      ArrowType(NilType, NilType),
                      Nil,
                      Map(funcWithArgs.name -> callable),
                      Map.empty
                    ),
                    tConfig
                  )
                )
                .map { script =>
                  val scriptVar = VarRaw("script", ScalarType.string)
                  RunOpts.execRun(
                    common,
                    AddFuncName,
                    aquaPath,
                    Nil,
                    scriptVar :: intervalArg :: Nil,
                    Map(
                      "script" -> ArgumentGetter(
                        scriptVar,
                        scalajs.js.Dynamic.literal("script" -> script).selectDynamic("script")
                      )
                    )
                  )
                }
                .sequence
            } yield {
              result
            }
          }.fold(
            errs =>
              Async[F].pure {
                errs.map(logger.error)
                cats.effect.ExitCode.Error
              },
            res =>
              res.map {
                case Validated.Invalid(errs) =>
                  errs.map(logger.error)
                  cats.effect.ExitCode.Error
                case Validated.Valid(ec) => ec

              }
          )
        )
      }
    }

  def scheduleOptsCompose[F[_]: Files: Async]
    : Opts[F[ValidatedNec[String, (Path, List[Path], FuncWithLiteralArgs, Path)]]] = {
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], ArgOpts.funcWithLiteralsOpt[F]).mapN {
      case (inputF, importF, funcWithLiteralsF) =>
        for {
          inputV <- inputF
          importV <- importF
          funcWithLiteralsV <- funcWithLiteralsF
          aquaPath <- PlatformOpts.getPackagePath[F](ScriptAqua)
        } yield {
          (inputV, importV, funcWithLiteralsV).mapN { case (i, im, f) =>
            (i, im, f, aquaPath)
          }
        }
    }
  }

  // Removes scheduled script from a node
  def remove[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "remove",
      header = "Remove a service from a remote peer"
    ) {
      (
        GeneralRunOptions.commonOptWithSecretKey,
        scriptIdOpt
      ).mapN { (common, scriptId) =>
        PlatformOpts.getPackagePath(ScriptAqua).flatMap { distAquaPath =>
          val args = LiteralRaw.quote(scriptId) :: Nil
          RunOpts.execRun(
            common,
            RemoveFuncName,
            distAquaPath,
            Nil,
            args
          )
        }
      }
    }

  // Print all scheduled scripts
  def list[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "list",
      header = "Print all scheduled scripts"
    ) {
      (
        GeneralRunOptions.commonOpt
      ).map { common =>
        PlatformOpts.getPackagePath(ScriptAqua).flatMap { distAquaPath =>
          RunOpts.execRun(
            common,
            ListFuncName,
            distAquaPath,
            Nil,
            Nil
          )
        }
      }
    }
}
