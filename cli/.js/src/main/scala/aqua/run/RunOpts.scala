package aqua.run

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.types.BottomType
import aqua.{AppOpts, AquaIO, FileOpts, FluenceOpts, LogFormatter}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
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

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  val dataOpt: Opts[js.Dynamic] =
    Opts.option[String]("data", "Argument map for aqua function in JSON format", "d").mapValidated {
      str =>
        Validated.catchNonFatal {
          JSON.parse(str)
        }.leftMap(t => NonEmptyList.one("Data isn't a valid JSON: " + t.getMessage))
    }

  val funcOpt: Opts[(String, List[ValueRaw])] =
    Opts
      .option[String]("func", "Function to call with args", "f")
      .mapValidated { str =>
        CallArrowExpr.funcOnly.parseAll(str) match {
          case Right(exprSpan) =>
            val expr = exprSpan.mapK(spanToId)

            val args = expr.args.collect {
              case Literal(value, ts) =>
                LiteralRaw(value, ts)
              case VarLambda(name, _) =>
                // TODO why BottomType?
                VarRaw(name.value, BottomType)
            }

            validNel((expr.funcName.value, args))

          case Left(err) => invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    args: List[ValueRaw],
    data: Option[js.Dynamic]
  ): ValidatedNec[String, Map[String, ArgumentGetter]] = {
    val vars = args.collect { case v @ VarRaw(_, _, _) =>
      v
    // one variable could be used multiple times
    }.distinctBy(_.name)

    data match {
      case None if vars.nonEmpty =>
        invalidNec("Function have non-literals, so, data should be presented")
      case None =>
        validNec(Map.empty)
      case Some(data) =>
        val services = vars.map { vm =>
          val arg = data.selectDynamic(vm.name)
          vm.name -> ArgumentGetter(vm, arg)
        }
        validNec(services.toMap)
    }
  }

  def dataFromFileOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    FileOpts.fileOpt(
      "data-path",
      "Path to file with arguments map in JSON format",
      "p",
      (path, str) => {
        Validated.catchNonFatal {
          JSON.parse(str)
        }.leftMap(t =>
          NonEmptyChain
            .one(s"Data in ${path.toString} isn't a valid JSON: " + t.getMessage)
        )
      }
    )
  }

  // get data from sources
  def getData(
    dataFromArgument: Option[js.Dynamic],
    dataFromFile: Option[js.Dynamic]
  ): ValidatedNec[String, Option[js.Dynamic]] = {
    (dataFromArgument, dataFromFile) match {
      case (Some(_), Some(_)) =>
        invalidNec("Pass either data argument or path to file with arguments")
      case _ => validNec(dataFromArgument.orElse(dataFromFile))
    }
  }

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
      funcOpt,
      AppOpts.wrapWithOption(dataOpt),
      AppOpts.wrapWithOption(dataFromFileOpt[F])
    ).mapN {
      case (
            common,
            inputF,
            importF,
            (func, args),
            dataFromArgument,
            dataFromFileF
          ) =>
        LogFormatter.initLogger(Some(common.logLevel))
        for {
          inputV <- inputF
          impsV <- importF
          dataFromFileV <- dataFromFileF
            .map(_.map(_.map(Some.apply)))
            .getOrElse(validNec(None).pure[F])
          resultV: ValidatedNec[String, F[Unit]] = inputV.andThen { input =>
            impsV.andThen { imps =>
              dataFromFileV.andThen { dataFromFile =>
                getData(dataFromArgument, dataFromFile).andThen { data =>
                  checkDataGetServices(args, data).andThen { getServices =>
                    valid(
                      RunCommand
                        .run(
                          func,
                          args,
                          input,
                          imps,
                          RunConfig(common, getServices, builtinServices),
                          transformConfigWithOnPeer(common.on)
                        )
                    )
                  }
                }
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
