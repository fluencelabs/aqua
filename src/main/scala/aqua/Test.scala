package aqua

import cats.effect.{IO, IOApp}
import cats.data.Validated

import scala.io.Source

object Test extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {
      def process(str: String) =
        Aqua.generate(str) match {
          case Validated.Valid(v) ⇒
            println(v)
            println(Console.GREEN + "Aqua script processed successfully" + Console.RESET)
          case Validated.Invalid(errs) ⇒
            errs.map(_.showForConsole(str)).map(println)
            println(Console.RED + s"Aqua script errored, total ${errs.length} problems found" + Console.RESET)
        }

      val script = Source.fromResource("generate.aqua").mkString
      process(script)

    }

}
