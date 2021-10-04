package aqua

import cats.Monad
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Files
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import aqua.RunCommand
import aqua.parser.expr.CallArrowExpr
import cats.effect.{ExitCode, IO}
import cats.~>
import cats.Id
import cats.data.{NonEmptyList, Validated}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser

import scala.concurrent.Future

object RunOpts {

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")
      .withDefault("/dns4/stage.fluence.dev/tcp/19002/wss/p2p/12D3KooWMigkP4jkVyufq5JnDJL6nXvyjeaDNpRfEZqQhsG3sYCU")

  val funcNameOpt: Opts[String] =
    Opts
      .option[String]("func", "Function to call with args", "f")

  def runOptions[F[_]: Monad: Files: AquaIO](implicit F: Future ~> F): Opts[F[cats.effect.ExitCode]] =
    (AppOpts.inputOpts[F], AppOpts.importOpts[F], multiaddrOpt, funcNameOpt).mapN { (i, imp, multiaddr, func) =>
      RunCommand.run(multiaddr, func).map(_ => cats.effect.ExitCode.Success)
    }

  def runCommand[F[_]: Monad: Files: AquaIO](implicit F: Future ~> F): Command[F[ExitCode]] = Command(
    name = "run",
    header = "Run a function from an aqua code"
  ) {
    runOptions
  }
}
