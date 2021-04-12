package aqua

import aqua.backend.ts.TypescriptFile
import aqua.io.{AquaFileError, AquaFiles, AquaScriptErrors, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.ScriptModel
import aqua.parser.lift.FileSpan
import aqua.semantics.{CompilerState, Semantics}
import cats.effect.{IO, IOApp}
import cats.data.Validated

import java.nio.file.Paths
import scala.io.Source

object Test extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {
      def process(str: String) = {
        AquaFiles
          .readAndResolve[IO, CompilerState.S[FileSpan.F]](
            Paths.get("./aqua"),
            LazyList(),
            ast => _.flatMap(_ => Semantics.astToState(ast))
          )
          .value
          .map {
            case Left(fileErrors) =>
              println(Console.RED + "File errors")
              println(fileErrors.toChain.toList.mkString("\n") + Console.RESET)

            case Right(modules) =>
              println(modules)
              Linker[FileModuleId, AquaFileError, CompilerState.S[FileSpan.F]](
                modules,
                ids => Unresolvable(ids.map(_.id.file.toString).mkString(" -> "))
              ) match {
                case Validated.Valid(files) ⇒
                  files.map { case (modId, proc) =>
                    proc.run(CompilerState()).value match {
                      case (proc, _) if proc.errors.nonEmpty =>
                        proc.errors.map(err => println(Console.RED + err + Console.RESET))
                      case (_, model: ScriptModel) =>
                        println(TypescriptFile(model).generateTS())
                      case (_, model) =>
                        println(Console.RED + "Unknown model: " + model)
                    }
                  }

                  println(Console.GREEN + "Aqua script processed successfully" + Console.RESET)
                case Validated.Invalid(errs) ⇒
                  errs
                    //.map(_.showForConsole(str))
                    .map(println)
                  println(
                    Console.RED + s"Aqua script errored, total ${errs.length} problems found" + Console.RESET
                  )
              }
          }
      }

      val script = Source.fromResource("for-par.aqua").mkString
      process(script)

    }

}
