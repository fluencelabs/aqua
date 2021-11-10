package aqua

import aqua.RunCommand
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{Literal, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.parser.lift.Span
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

import scala.concurrent.{ExecutionContext, Future}

object RunOpts {

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")
      .withDefault(
        "/dns4/kras-00.fluence.dev/tcp/19001/wss/p2p/12D3KooWR4cv1a8tv7pps4HH6wePNaK6gf1Hww5wcCMzeWxyNw51"
      )

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  val funcNameOpt: Opts[(String, List[Literal[Id]])] =
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
              Validated.invalidNel("Function must have only literals as variables")
            } else {
              val args = expr.args.collect { case l @ Literal(_, _) =>
                l
              }

              Validated.validNel((expr.funcName.value, args))
            }
          case Left(err) => Validated.invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  def runOptions[F[_]: Files: AquaIO: Async](implicit
    ec: ExecutionContext
  ): Opts[F[cats.effect.ExitCode]] =
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], multiaddrOpt, funcNameOpt).mapN {
      case (inputF, importF, multiaddr, (func, args)) =>
        for {
          inputV <- inputF
          impsV <- importF
          result <- inputV.fold(
            _ => cats.effect.ExitCode.Error.pure[F],
            { input =>
              impsV.fold(
                _ => cats.effect.ExitCode.Error.pure[F],
                { imps =>
                  RunCommand
                    .run(multiaddr, func, args, input, imps)
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
