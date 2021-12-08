package aqua.run

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.types.BottomType
import aqua.{AppOpts, AquaIO, ArgGetterService, LogFormatter}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
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

  val timeoutOpt: Opts[Int] =
    Opts
      .option[Int]("timeout", "Request timeout in milliseconds", "t")
      .withDefault(7000)

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")
      .withDefault(
        "/dns4/kras-00.fluence.dev/tcp/19001/wss/p2p/12D3KooWR4cv1a8tv7pps4HH6wePNaK6gf1Hww5wcCMzeWxyNw51"
      )

  val secretKeyOpt: Opts[Array[Byte]] =
    Opts
      .option[String]("sk", "Ed25519 32-byte secret key in base64", "s")
      .mapValidated { s =>
        val decoder = Base64.getDecoder
        Validated.catchNonFatal {
          decoder.decode(s)
        }.leftMap(t => NonEmptyList.one("secret key isn't a valid base64 string: " + t.getMessage))
      }

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  val printAir: Opts[Boolean] =
    Opts
      .flag("print-air", "Prints generated AIR code before function execution")
      .map(_ => true)
      .withDefault(false)

  def dataFromFileOpt[F[_]: Monad: Files: Concurrent]
    : Opts[F[ValidatedNec[String, Option[js.Dynamic]]]] =
    Opts
      .option[String]("data-path", "Path to file with arguments in JSON format", "p")
      .map { str =>
        val p = Path(str)
        Files[F]
          .exists(p)
          .flatMap { exists =>
            if (exists)
              Files[F].isRegularFile(p).flatMap { isFile =>
                if (isFile) {
                  Files[F]
                    .readAll(p)
                    .through(fs2.text.utf8.decode)
                    .fold(List.empty[String]) { case (acc, str) => str :: acc }
                    .map(_.mkString(""))
                    .map { jsonStr =>
                      Validated.catchNonFatal {
                        JSON.parse(jsonStr)
                      }.leftMap(t =>
                        NonEmptyChain
                          .one(s"Data in ${p.toString} isn't a valid JSON: " + t.getMessage)
                      )
                    }
                    .compile
                    .last
                    .map(_.map(_.map(v => Some(v))).getOrElse(validNec(None)))
                } else {
                  invalidNec(s"Path '${p.toString}' is not a file").pure[F]
                }
              }
            else {
              invalidNec(s"There is no path '${p.toString}'").pure[F]
            }
          }
      }

  val dataOpt: Opts[js.Dynamic] =
    Opts.option[String]("data", "Data for aqua function in json", "d").mapValidated { str =>
      Validated.catchNonFatal {
        JSON.parse(str)
      }.leftMap(t => NonEmptyList.one("Data isn't a valid JSON: " + t.getMessage))
    }

  val funcOpt: Opts[(String, List[ValueModel])] =
    Opts
      .option[String]("func", "Function to call with args", "f")
      .mapValidated { str =>
        CallArrowExpr.funcOnly.parseAll(str) match {
          case Right(exprSpan) =>
            val expr = exprSpan.mapK(spanToId)

            val args = expr.args.collect {
              case Literal(value, ts) =>
                LiteralModel(value, ts)
              case VarLambda(name, _) =>
                VarModel(name.value, BottomType)
            }

            validNel((expr.funcName.value, args))

          case Left(err) => invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    args: List[ValueModel],
    data: Option[js.Dynamic]
  ): ValidatedNec[String, Map[String, ArgGetterService]] = {
    val vars = args.collect { case v @ VarModel(_, _, _) =>
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
          vm.name -> ArgGetterService.create(vm, arg)
        }
        validNec(services.toMap)
    }
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

  def runOptions[F[_]: Files: AquaIO: Async](implicit
    ec: ExecutionContext
  ): Opts[F[cats.effect.ExitCode]] =
    (
      AppOpts.inputOpts[F],
      AppOpts.importOpts[F],
      multiaddrOpt,
      funcOpt,
      timeoutOpt,
      AppOpts.logLevelOpt,
      printAir,
      AppOpts.wrapWithOption(secretKeyOpt),
      AppOpts.wrapWithOption(dataOpt),
      AppOpts.wrapWithOption(dataFromFileOpt[F])
    ).mapN {
      case (
            inputF,
            importF,
            multiaddr,
            (func, args),
            timeout,
            logLevel,
            printAir,
            secretKey,
            dataFromArgument,
            dataFromFileF
          ) =>
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(logLevel))
          .replace()

        for {
          inputV <- inputF
          impsV <- importF
          dataFromFileV <- dataFromFileF.getOrElse(validNec(None).pure[F])
          resultV: ValidatedNec[String, F[Unit]] = inputV.andThen { input =>
            impsV.andThen { imps =>
              dataFromFileV.andThen { dataFromFile =>
                getData(dataFromArgument, dataFromFile).andThen { data =>
                  checkDataGetServices(args, data).andThen { services =>
                    valid(
                      RunCommand
                        .run(
                          multiaddr,
                          func,
                          args,
                          input,
                          imps,
                          RunConfig(timeout, logLevel, printAir, secretKey, services)
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
