package aqua

import aqua.context.Types
import aqua.parser.ast.Ast
import aqua.parser.lift.Span
import cats.effect.{IO, IOApp}
import cats.data.{NonEmptyList, Validated}

import scala.io.Source

object Main extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {
      def tryParse(str: String) =
        Aqua.parse(str) match {
          case Validated.Valid(v) ⇒
            println(v)
            println(Console.GREEN + "Aqua script processed successfully" + Console.RESET)
          case Validated.Invalid(errs) ⇒
            errs.map(_.showForConsole(str)).map(println)
            println(Console.RED + s"Aqua script errored, total ${errs.length} problems found" + Console.RESET)
        }

      val experimental = Source.fromResource("experimental.aqua").mkString
      //tryParse(experimental)

      println(
        Ast
          .fromString[Span.F](experimental)
          .leftMap(_.map(_.showForConsole(experimental)))
          .map(_.tree.map(_.toString + "\n").forceAll)
      )
    }

}
