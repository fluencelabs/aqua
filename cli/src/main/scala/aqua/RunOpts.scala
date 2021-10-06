package aqua

import aqua.RunCommand
import aqua.parser.expr.CallArrowExpr
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
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
      .withDefault("/dns4/stage.fluence.dev/tcp/19002/wss/p2p/12D3KooWMigkP4jkVyufq5JnDJL6nXvyjeaDNpRfEZqQhsG3sYCU")

  val funcNameOpt: Opts[String] =
    Opts
      .option[String]("func", "Function to call with args", "f")

  def runOptions[F[_]: Monad: Files: AquaIO: Async](implicit ec: ExecutionContext): Opts[F[cats.effect.ExitCode]] =
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], multiaddrOpt, funcNameOpt).mapN { (inputF, importF, multiaddr, func) =>
      for {
        inputV <- inputF
        impsV <- importF
        result <- inputV.fold(_ => cats.effect.ExitCode.Error.pure[F], { input =>
          impsV.fold(_ => cats.effect.ExitCode.Error.pure[F], { imps =>
            RunCommand.run(multiaddr, func, input, imps).map(_ => cats.effect.ExitCode.Success)
          })
        })
      } yield {
        result
      }

    }

  def runCommand[F[_]: Monad: Files: AquaIO: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] = Command(
    name = "run",
    header = "Run a function from an aqua code"
  ) {
    runOptions
  }
}
