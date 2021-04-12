package aqua

import aqua.io.{AquaFileError, AquaFiles, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.ScriptModel
import aqua.parser.lexer.Token
import aqua.parser.lift.FileSpan
import aqua.semantics.{CompilerState, Semantics}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.kernel.Concurrent
import fs2.io.file.Files
import cats.syntax.monoid._
import cats.syntax.functor._

import java.nio.file.Path

object AquaCompiler {
  case class Prepared(target: String => Path, model: ScriptModel)

  def prepareFiles[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path
  ): F[ValidatedNec[String, Chain[Prepared]]] =
    AquaFiles
      .readAndResolve[F, CompilerState.S[FileSpan.F]](
        srcPath,
        imports,
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
          Validated.invalid(fileErrors.map(_.showForConsole))

        case Right(modules) =>
          Linker[FileModuleId, AquaFileError, CompilerState.S[FileSpan.F]](
            modules,
            ids => Unresolvable(ids.map(_.id.file.toString).mkString(" -> "))
          ) match {
            case Validated.Valid(files) ⇒
              val (errs, preps) =
                files.toSeq.foldLeft[(Chain[String], Chain[Prepared])]((Chain.empty, Chain.empty)) {
                  case ((errs, preps), (modId, proc)) =>
                    proc.run(CompilerState()).value match {
                      case (proc, _) if proc.errors.nonEmpty =>
                        (errs ++ showProcErrors(proc.errors), preps)

                      case (_, model: ScriptModel) =>
                        (errs, preps :+ Prepared(modId.targetPath(srcPath, targetPath, _), model))

                      case (_, model) =>
                        (
                          errs.append(Console.RED + "Unknown model: " + model + Console.RESET),
                          preps
                        )
                    }
                }
              NonEmptyChain
                .fromChain(errs)
                .fold(Validated.validNec[String, Chain[Prepared]](preps))(Validated.invalid)

            case Validated.Invalid(errs) ⇒
              Validated.invalid(
                errs
                  .map(_.showForConsole)
              )
          }
      }

  def showProcErrors(
    errors: Chain[(Token[FileSpan.F], String)]
  ): Chain[String] =
    errors.map(err =>
      err._1.unit._1
        .focus(1)
        .map(_.toConsoleStr(err._2, Console.CYAN))
        .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
    )

}
