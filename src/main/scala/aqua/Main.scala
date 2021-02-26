package aqua

import cats.effect.{IO, IOApp}
import cats.syntax.show._
import aqua.ir._
import cats.data.{Kleisli, Validated}

import scala.io.Source

object Main extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {
      def tryParse(str: String) =
        Aqua.parse(str) match {
          case Validated.Valid(v) ⇒ println(v)
          case Validated.Invalid(errs) ⇒
            errs.map(_.showForConsole(str)).map(println)
        }

      val experimental = Source.fromResource("experimental.aqua").mkString
      tryParse(experimental)
    }

}
