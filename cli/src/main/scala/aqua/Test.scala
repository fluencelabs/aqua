package aqua

import aqua.backend.ts.TypescriptFile
import aqua.io.{AquaFileError, AquaFiles, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.ScriptModel
import aqua.parser.lexer.Token
import aqua.parser.lift.FileSpan
import aqua.semantics.{CompilerState, Semantics}
import cats.effect.{IO, IOApp}
import cats.data.{Chain, Validated}
import cats.syntax.monoid._

import java.nio.file.Paths

object Test extends IOApp.Simple {

  override def run: IO[Unit] = {
    AquaFiles
      .readAndResolve[IO, CompilerState.S[FileSpan.F]](
        Paths.get("./aqua-src"),
        LazyList(Paths.get("./aqua")),
        ast =>
          _.flatMap(m => {
            //println(Console.YELLOW + "running for ast " + Console.RESET);
            for {
              y <- Semantics.astToState(ast)
            } yield m |+| y
          })
      )
      .value
      .map {
        case Left(fileErrors) =>
          println(Console.RED + "File errors")
          println(fileErrors.toChain.map(_.showForConsole).toList.mkString("\n") + Console.RESET)

        case Right(modules) =>
          Linker[FileModuleId, AquaFileError, CompilerState.S[FileSpan.F]](
            modules,
            ids => Unresolvable(ids.map(_.id.file.toString).mkString(" -> "))
          ) match {
            case Validated.Valid(files) ⇒
              files.map { case (modId, proc) =>
                proc.run(CompilerState()).value match {
                  case (proc, _) if proc.errors.nonEmpty =>
                    showProcErrors(proc.errors)

                    println(Console.RED + "Script errored " + Console.RESET)
                  case (_, model: ScriptModel) =>
                    // TODO write to target file
                    println(
                      modId.targetPath(
                        Paths.get("./aqua-src"),
                        Paths.get("./target"),
                        "ts"
                      ) + "\n" + TypescriptFile(model).generateTS()
                    )
                    println(Console.GREEN + "Aqua script processed successfully" + Console.RESET)

                  case (_, model) =>
                    println(Console.RED + "Unknown model: " + model + Console.RESET)
                }
              }

            case Validated.Invalid(errs) ⇒
              errs
                .map(_.showForConsole)
                .map(println)
              println(
                Console.RED + s"Aqua script errored, total ${errs.length} problems found" + Console.RESET
              )
          }
      }
  }

  def showProcErrors(errors: Chain[(Token[FileSpan.F], String)]): Unit =
    errors
      .map(err =>
        err._1.unit._1
          .focus(1)
          .map(_.toConsoleStr(err._2, Console.CYAN))
          .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
      )
      .map(println)

}
