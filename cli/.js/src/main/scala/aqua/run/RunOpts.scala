package aqua.run

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.types.BottomType
import aqua.{AppOpts, AquaIO, ArgGetterService, LogFormatter}
import cats.data.{NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import cats.effect.kernel.Async
import cats.effect.{ExitCode, IO}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{~>, Id, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Files
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
          case Right(f) =>
            val expr = f.mapK(spanToId)

            val args = expr.args.collect {
              case Literal(value, ts) =>
                LiteralModel(value, ts)
              case VarLambda(name, _) =>
                VarModel(name.value, BottomType)
            }

            Validated.validNel((expr.funcName.value, args))

          case Left(err) => Validated.invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  def checkDataGetServices(
    args: List[ValueModel],
    data: Option[js.Dynamic]
  ): ValidatedNec[String, List[ArgGetterService]] = {
    val vars = args.collect { case v @ VarModel(_, _, _) =>
      v
    }

    data match {
      case None if vars.nonEmpty =>
        Validated.invalidNec("Function have non-literals, so, data should be presented")
      case None =>
        Validated.validNec(Nil)
      case Some(data) =>
        val services = vars.map { vm =>
          ArgGetterService.create(vm, data.selectDynamic(vm.name))
        }
        Validated.validNec(services)
    }
  }

  def foldValid[F[_]: Async, T](
    validated: ValidatedNec[String, T],
    f: T => F[cats.effect.ExitCode]
  ): F[cats.effect.ExitCode] = {
    validated.fold(
      errs => {
        Async[F].pure {
          errs.map(logger.error)
          cats.effect.ExitCode.Error
        }
      },
      f
    )
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
      AppOpts.wrapWithOption(dataOpt)
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
            data
          ) =>
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(logLevel))
          .replace()

        for {
          inputV <- inputF
          impsV <- importF
          resultV: ValidatedNec[String, F[Unit]] = inputV.andThen { input =>
            impsV.andThen { imps =>
              checkDataGetServices(args, data).andThen { services =>
                Validated.valid(
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
