package aqua

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypescriptFile
import aqua.io.{AquaFileError, AquaFiles, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.transform.BodyConfig
import aqua.model.{ConstantModel, ScriptModel}
import aqua.parser.lexer.Token
import aqua.parser.lift.FileSpan
import aqua.semantics.{CompilerState, Semantics}
import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.effect.kernel.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.syntax.show._
import fs2.io.file.Files
import fs2.text
import wvlet.log.LogSupport

import java.nio.file.Path

object AquaCompiler extends LogSupport {
  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  case class Prepared(srcFile: Path, srcDir: Path, model: ScriptModel) {

    def hasOutput(target: CompileTarget): Boolean = target match {
      case _ => model.funcs.nonEmpty
    }

    def targetPath(ext: String): Validated[Throwable, Path] =
      Validated.catchNonFatal {
        val fileName = srcFile.getFileName
        if (fileName == null) {
          throw new Exception(s"Unexpected: 'fileName' is null in path $srcFile")
        } else {
          srcDir.resolve(fileName.toString.stripSuffix(".aqua") + s".$ext")
        }
      }
  }

  def prepareFiles[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path,
    constants: Chain[ConstantModel]
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
              val (errs, preps, _) =
                files.toSeq.foldLeft[(Chain[String], Chain[Prepared], Option[ScriptModel])](
                  (Chain.empty, Chain.empty, None)
                ) { case ((errs, preps, lastModel), (modId, proc)) =>
                  proc
                    // TODO: uncomment after linker updated
//                    .run(CompilerState.fromModels(lastModel.fold(Chain.empty[Model])(_.models)))
                    .run(CompilerState.fromModels(constants))
                    .value match {
                    case (proc, _) if proc.errors.nonEmpty =>
                      (errs ++ showProcErrors(proc.errors), preps, lastModel)

                    case (_, model: ScriptModel) =>
                      val src = Validated.catchNonFatal {
                        targetPath.toAbsolutePath
                          .normalize()
                          .resolve(
                            srcPath.toAbsolutePath
                              .normalize()
                              .relativize(modId.file.toAbsolutePath.normalize())
                          )
                      }
                      src match {
                        case Validated.Invalid(t) => (errs :+ t.getMessage, preps)
                        case Validated.Valid(s) => (errs, preps :+ Prepared(s, srcPath, model))
                      }

                    case (_, model) =>
                      (
                        errs.append(Console.RED + "Unknown model: " + model + Console.RESET),
                        preps,
                        lastModel
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
        .focus(2)
        .map(_.toConsoleStr(err._2, Console.CYAN))
        .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
    )

  def compileFilesTo[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path,
    compileTo: CompileTarget,
    bodyConfig: BodyConfig,
    constants: Chain[ConstantModel]
  ): F[ValidatedNec[String, Chain[String]]] =
    prepareFiles(srcPath, imports, targetPath)
      .map(_.map(_.filter { p =>
        val hasOutput = p.hasOutput(compileTo)
        if (!hasOutput) info(s"Source ${p.srcFile}: compilation OK (nothing to emit)")
        else info(s"Source ${p.srcFile}: compilation OK (${p.model.funcs.length} functions)")
        hasOutput
      }))
      .flatMap[ValidatedNec[String, Chain[String]]] {
        case Validated.Invalid(e) =>
          Applicative[F].pure(Validated.invalid(e))
        case Validated.Valid(preps) =>
          (compileTo match {
            case TypescriptTarget =>
              preps.map { p =>
                val tpV = p.targetPath("ts")
                tpV match {
                  case Invalid(t) =>
                    EitherT.pure(t.getMessage)
                  case Valid(tp) =>
                    writeFile(tp, TypescriptFile(p.model).generateTS(bodyConfig))
                }

              }

            // TODO add function name to AirTarget class
            case AirTarget =>
              preps
                .flatMap(p =>
                  p.model.resolveFunctions
                    .map(fc => FuncAirGen(fc).generateAir(bodyConfig).show)
                    .map { generated =>
                      val tpV = p.targetPath("ts")
                      tpV match {
                        case Invalid(t) =>
                          EitherT.pure(t.getMessage)
                        case Valid(tp) =>
                          writeFile(
                            tp,
                            generated
                          )
                      }
                    }
                )

          }).foldLeft(
            EitherT.rightT[F, NonEmptyChain[String]](Chain.empty[String])
          ) { case (accET, writeET) =>
            EitherT(for {
              a <- accET.value
              w <- writeET.value
            } yield (a, w) match {
              case (Left(errs), Left(err)) => Left(errs :+ err)
              case (Right(res), Right(_)) => Right(res)
              case (Left(errs), _) => Left(errs)
              case (_, Left(err)) => Left(NonEmptyChain.of(err))
            })
          }.value
            .map(Validated.fromEither)

      }

  def writeFile[F[_]: Files: Concurrent](file: Path, content: String): EitherT[F, String, Unit] =
    EitherT.right[String](Files[F].deleteIfExists(file)) >>
      EitherT[F, String, Unit](
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
          .map(_ => Right(()))
      )

}
