package aqua

import aqua.context.Types
import aqua.ast.algebra.names.NameOp
import aqua.ast.{Ast, Expr}
import aqua.parser.lift.Span
import cats.Eval
import cats.effect.{IO, IOApp}
import cats.data.Validated

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

      val script = Source.fromResource("typecheck.aqua").mkString
      //tryParse(experimental)

      val ast = Ast
        .fromString[Span.F](script)
        .andThen(
          Compiler
            .compile[Span.F](_)
            .leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2)))
        )
        .leftMap(_.map(_.showForConsole(script)).map(println))

      ast.map(println(_))

    }

}
