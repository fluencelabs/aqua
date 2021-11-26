package aqua.run

import aqua.model.LiteralModel
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
import aqua.{AppOpts, AquaIO, LogFormatter}
import cats.data.{NonEmptyList, Validated}
import cats.effect.kernel.Async
import cats.effect.{ExitCode, IO}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Id, Monad, ~>}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Files
import scribe.Logging

import java.util.Base64
import scala.concurrent.ExecutionContext

object RunOpts extends Logging {

  val timeoutOpt: Opts[Int] =
    Opts.option[Int]("timeout", "Request timeout in milliseconds", "t")
      .withDefault(7000)
  
  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")
      .withDefault(
        "/dns4/kras-00.fluence.dev/tcp/19001/wss/p2p/12D3KooWR4cv1a8tv7pps4HH6wePNaK6gf1Hww5wcCMzeWxyNw51"
      )

  val secretKeyOpt: Opts[Array[Byte]] =
    Opts
      .option[String]("sk", "Secret key in base64. Requests will be initialized with this key.", "s")
      .mapValidated { s =>
        val decoder = Base64.getDecoder
        Validated.catchNonFatal {
          decoder.decode(s)
        }.leftMap(t => NonEmptyList.one("Secret key must be valid base64 string: " + t.getMessage))
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


  val funcOpt: Opts[(String, List[LiteralModel])] =
    Opts
      .option[String]("func", "Function to call with args", "f")
      .mapValidated { str =>
        CallArrowExpr.funcOnly.parseAll(str) match {
          case Right(f) =>
            val expr = f.mapK(spanToId)
            val hasVars = expr.args.exists {
              case VarLambda(_, _) => true
              case _ => false
            }
            if (hasVars) {
              Validated.invalidNel("Function can have only literal arguments, no variables or constants allowed at the moment")
            } else {
              val args = expr.args.collect { case l @ Literal(_, _) =>
                LiteralModel(l.value, l.ts)
              }

              Validated.validNel((expr.funcName.value, args))
            }
          case Left(err) => Validated.invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  def runOptions[F[_]: Files: AquaIO: Async](implicit
    ec: ExecutionContext
  ): Opts[F[cats.effect.ExitCode]] =
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], multiaddrOpt, funcOpt, timeoutOpt, AppOpts.logLevelOpt, printAir, AppOpts.wrapWithOption(secretKeyOpt)).mapN {
      case (inputF, importF, multiaddr, (func, args), timeout, logLevel, printAir, secretKey) =>

        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(logLevel))
          .replace()

        for {
          inputV <- inputF
          impsV <- importF
          result <- inputV.fold(
            errs => {
              Async[F].pure {
                errs.map(logger.error)
                cats.effect.ExitCode.Error
              }
            },
            { input =>
              impsV.fold(
                errs => {
                  Async[F].pure {
                    errs.map(logger.error)
                    cats.effect.ExitCode.Error
                  }
                },
                { imps =>
                  RunCommand
                    .run(multiaddr, func, args, input, imps, RunConfig(timeout, logLevel, printAir, secretKey))
                    .map(_ => cats.effect.ExitCode.Success)
                }
              )
            }
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
