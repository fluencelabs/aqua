package aqua

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypescriptFile
import aqua.backend.js.JavaScriptFile
import aqua.io.{AquaFileError, AquaFiles, FileModuleId, Unresolvable}
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import aqua.parser.lift.FileSpan
import aqua.semantics.{RulesViolated, SemanticError, Semantics}
import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.effect.kernel.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import fs2.io.file.Files
import fs2.text
import wvlet.log.LogSupport

import java.nio.file.Path

object AquaCompiler extends LogSupport {
  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object JavaScriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  case class Prepared(modFile: Path, srcPath: Path, targetPath: Path, context: AquaContext) {

    def hasOutput(target: CompileTarget): Boolean = target match {
      case _ => context.funcs.nonEmpty
    }

    def targetPath(ext: String): Validated[Throwable, Path] =
      Validated.catchNonFatal {
        val srcDir = if (srcPath.toFile.isDirectory) srcPath else srcPath.getParent
        val srcFilePath = srcDir.toAbsolutePath
          .normalize()
          .relativize(modFile.toAbsolutePath.normalize())

        val targetAqua =
          targetPath.toAbsolutePath
            .normalize()
            .resolve(
              srcFilePath
            )

        val fileName = targetAqua.getFileName
        if (fileName == null) {
          throw new Exception(s"Unexpected: 'fileName' is null in path $targetAqua")
        } else {
          // rename `.aqua` file name to `.ext`
          targetAqua.getParent.resolve(fileName.toString.stripSuffix(".aqua") + s".$ext")
        }
      }
  }

  def prepareFiles[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path
  ): F[ValidatedNec[String, Chain[Prepared]]] =
    AquaFiles
      .readAndResolve[F, ValidatedNec[SemanticError[FileSpan.F], AquaContext]](
        srcPath,
        imports,
        ast => _.andThen(ctx => Semantics.process(ast, ctx))
      )
      .value
      .map {
        case Left(fileErrors) =>
          Validated.invalid(fileErrors.map(_.showForConsole))

        case Right(modules) =>
          Linker[FileModuleId, AquaFileError, ValidatedNec[SemanticError[FileSpan.F], AquaContext]](
            modules,
            ids => Unresolvable(ids.map(_.id.file.toString).mkString(" -> "))
          ) match {
            case Validated.Valid(files) ⇒
              val (errs, _, preps) =
                files.toSeq.foldLeft[(Chain[String], Set[String], Chain[Prepared])](
                  (Chain.empty, Set.empty, Chain.empty)
                ) { case ((errs, errsSet, preps), (modId, proc)) =>
                  proc.fold(
                    es => {
                      val newErrs = showProcErrors(es.toChain).filterNot(errsSet.contains)
                      (errs ++ newErrs, errsSet ++ newErrs.iterator, preps)
                    },
                    c => (errs, errsSet, preps :+ Prepared(modId.file, srcPath, targetPath, c))
                  )
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
    errors: Chain[SemanticError[FileSpan.F]]
  ): Chain[String] =
    errors.map {
      case RulesViolated(token, hint) =>
        token.unit._1
          .focus(2)
          .map(_.toConsoleStr(hint, Console.CYAN))
          .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
      case _ =>
        "Semantic error"
    }

  def compileFilesTo[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path,
    compileTo: CompileTarget,
    bodyConfig: BodyConfig
  ): F[ValidatedNec[String, Chain[String]]] =
    prepareFiles(srcPath, imports, targetPath)
      .map(_.map(_.filter { p =>
        val hasOutput = p.hasOutput(compileTo)
        if (!hasOutput) info(s"Source ${p.srcPath}: compilation OK (nothing to emit)")
        hasOutput
      }))
      .flatMap[ValidatedNec[String, Chain[String]]] {
        case Validated.Invalid(e) =>
          Applicative[F].pure(Validated.invalid(e))
        case Validated.Valid(preps) =>
          (compileTo match {
            case TypescriptTarget =>
              preps.map { p =>
                p.targetPath("ts") match {
                  case Invalid(t) =>
                    EitherT.pure(t.getMessage)
                  case Valid(tp) =>
                    writeFile(tp, TypescriptFile(p.context).generateTS(bodyConfig)).flatTap { _ =>
                      EitherT.pure(
                        Validated.catchNonFatal(
                          info(
                            s"Result ${tp.toAbsolutePath}: compilation OK (${p.context.funcs.size} functions)"
                          )
                        )
                      )
                    }
                }

              }

            case JavaScriptTarget =>
              preps.map { p =>
                p.targetPath("js") match {
                  case Invalid(t) =>
                    EitherT.pure(t.getMessage)
                  case Valid(tp) =>
                    writeFile(tp, JavaScriptFile(p.context).generateJS(bodyConfig)).flatTap { _ =>
                      EitherT.pure(
                        Validated.catchNonFatal(
                          info(
                            s"Result ${tp.toAbsolutePath}: compilation OK (${p.context.funcs.size} functions)"
                          )
                        )
                      )
                    }
                }

              }

            // TODO add function name to AirTarget class
            case AirTarget =>
              preps
                .flatMap(p =>
                  Chain
                    .fromSeq(p.context.funcs.values.toSeq)
                    .map(fc => fc.funcName -> FuncAirGen(fc).generateAir(bodyConfig).show)
                    .map { case (fnName, generated) =>
                      val tpV = p.targetPath(fnName + ".air")
                      tpV match {
                        case Invalid(t) =>
                          EitherT.pure(t.getMessage)
                        case Valid(tp) =>
                          writeFile(
                            tp,
                            generated
                          ).flatTap { _ =>
                            EitherT.pure(
                              Validated.catchNonFatal(
                                info(
                                  s"Result ${tp.toAbsolutePath}: compilation OK (${p.context.funcs.size} functions)"
                                )
                              )
                            )
                          }
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
