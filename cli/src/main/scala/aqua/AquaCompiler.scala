package aqua

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypescriptFile
import aqua.io.{AquaFileError, AquaFiles, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.ScriptModel
import aqua.parser.lexer.Token
import aqua.parser.lift.FileSpan
import aqua.semantics.{CompilerState, Semantics}
import cats.Applicative
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.kernel.Concurrent
import fs2.io.file.Files
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.show._
import fs2.text

import java.nio.file.Path

object AquaCompiler {
  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

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

  def compileFilesTo[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path,
    compileTo: CompileTarget
  ): F[ValidatedNec[String, Chain[String]]] =
    prepareFiles(srcPath, imports, targetPath).flatMap[ValidatedNec[String, Chain[String]]] {
      case Validated.Invalid(e) =>
        Applicative[F].pure(Validated.invalid(e))
      case Validated.Valid(preps) =>
        (compileTo match {
          case TypescriptTarget =>
            preps
              .map(p => writeFile(p.target("ts"), TypescriptFile(p.model).generateTS()))

          // TODO add function name to AirTarget class
          case AirTarget =>
            preps
              .map(p =>
                writeFile(
                  p.target("air"),
                  p.model.resolveFunctions
                    .map(FuncAirGen)
                    .map(g =>
                      // add function name before body
                      s";; function name: ${g.func.funcName}\n\n" + g.generateAir.show
                    )
                    .toList
                    .mkString("\n\n\n")
                )
              )

        }).foldLeft(
          EitherT.rightT[F, NonEmptyChain[String]](Chain.empty[String])
        ) { case (accET, writeET) =>
          EitherT(for {
            a <- accET.value
            w <- writeET.value
          } yield (a, w) match {
            case (Left(errs), Left(err)) => Left(errs :+ err)
            case (Right(res), Right(r)) => Right(res :+ r)
            case (Left(errs), _) => Left(errs)
            case (_, Left(err)) => Left(NonEmptyChain.of(err))
          })
        }.value
          .map(Validated.fromEither)

    }

  def writeFile[F[_]: Files: Concurrent](file: Path, content: String): EitherT[F, String, String] =
    EitherT.right[String](Files[F].deleteIfExists(file)) >>
      EitherT[F, String, String](
        fs2.Stream
          .emit(
            content
          )
          .through(text.utf8Encode)
          .through(Files[F].writeAll(file))
          .attempt
          .map { e =>
            e.left
              .map(t => s"Error on writing file $file" + t)
          }
          .compile
          .drain
          .map(_ => Right(s"Compiled $file"))
      )

}
