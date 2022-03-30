package aqua.config

import aqua.js.{FluenceEnvironment, FluenceNode}
import cats.Applicative
import cats.data.Validated
import cats.data.Validated.{invalidNel, validNel}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import com.monovore.decline.{Command, Opts}

import scala.scalajs.js

object ConfigOpts {

  def command[F[_]: Async]: Command[F[ExitCode]] =
    Command(name = "config", header = "Aqua CLI configuration") {
      Opts.subcommands(
        listPeers
      )
    }

  val Krasnodar = "krasnodar"
  val Stage = "stage"
  val TestNet = "testnet"

  def envArg: Opts[js.Array[FluenceNode]] =
    Opts
      .argument[String](s"$Krasnodar | $Stage | $TestNet")
      .withDefault(Krasnodar)
      .mapValidated {
        case Krasnodar =>
          validNel(FluenceEnvironment.krasnodar)
        case TestNet =>
          validNel(FluenceEnvironment.testnet)
        case Stage =>
          validNel(FluenceEnvironment.stage)
        case e =>
          invalidNel(
            s"There is no environment '$e' in our list. Use this: '$Krasnodar', '$TestNet', '$Stage'"
          )
      }

  def listPeers[F[_]: Applicative]: Command[F[ExitCode]] =
    Command(
      name = "default_peers",
      header = "List addresses of default peers in Fluence network"
    ) {
      envArg.map { env =>
        println(env.toList.map(n => n.multiaddr).mkString("\n"))
        ExitCode.Success.pure[F]
      }
    }
}
