package aqua

import aqua.backend.Backend
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.io._
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import aqua.parser.lift.FileSpan
import aqua.semantics.{RulesViolated, SemanticError, Semantics}
import cats.data._
import cats.effect.kernel.Concurrent
import cats.kernel.Monoid
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import fs2.io.file.Files
import wvlet.log.LogSupport

import java.nio.file.Path

object AquaCompiler extends LogSupport {
  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object JavaScriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  private def gatherPreparedFiles(
    srcPath: Path,
    targetPath: Path,
    files: Map[FileModuleId, ValidatedNec[SemanticError[FileSpan.F], AquaContext]]
  ): ValidatedNec[String, Chain[Prepared]] = {
    val (errs, _, preps) = files.toSeq.foldLeft[(Chain[String], Set[String], Chain[Prepared])](
      (Chain.empty, Set.empty, Chain.empty)
    ) { case ((errs, errsSet, preps), (modId, proc)) =>
      proc.fold(
        es => {
          val newErrs = showProcErrors(es.toChain).filterNot(errsSet.contains)
          (errs ++ newErrs, errsSet ++ newErrs.iterator, preps)
        },
        c => {
          Prepared(modId.file, srcPath, targetPath, c) match {
            case Validated.Valid(p) ⇒
              (errs, errsSet, preps :+ p)
            case Validated.Invalid(err) ⇒
              (errs :+ err.getMessage, errsSet, preps)
          }

        }
      )
    }
    NonEmptyChain
      .fromChain(errs)
      .fold(Validated.validNec[String, Chain[Prepared]](preps))(Validated.invalid)
  }

  /**
   * Create a structure that will be used to create output by a backend
   */
  def prepareFiles[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path
  )(implicit aqum: Monoid[AquaContext]): F[ValidatedNec[String, Chain[Prepared]]] =
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
              gatherPreparedFiles(srcPath, targetPath, files)

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

  def targetToBackend(target: CompileTarget): Backend = {
    target match {
      case TypescriptTarget =>
        TypeScriptBackend
      case JavaScriptTarget =>
        JavaScriptBackend
      case AirTarget =>
        AirBackend
    }
  }

  private def gatherResults[F[_]: Monad](
    results: List[EitherT[F, String, Unit]]
  ): F[Validated[NonEmptyChain[String], Chain[String]]] = {
    results
      .foldLeft(
        EitherT.rightT[F, NonEmptyChain[String]](Chain.empty[String])
      ) { case (accET, writeET) =>
        EitherT(for {
          acc <- accET.value
          writeResult <- writeET.value
        } yield (acc, writeResult) match {
          case (Left(errs), Left(err)) => Left(errs :+ err)
          case (Right(res), Right(_)) => Right(res)
          case (Left(errs), _) => Left(errs)
          case (_, Left(err)) => Left(NonEmptyChain.of(err))
        })
      }
      .value
      .map(Validated.fromEither)
  }

  def compileFilesTo[F[_]: Files: Concurrent](
    srcPath: Path,
    imports: LazyList[Path],
    targetPath: Path,
    compileTo: CompileTarget,
    bodyConfig: BodyConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    import bodyConfig.aquaContextMonoid
    prepareFiles(srcPath, imports, targetPath)
      .map(_.map(_.filter { p =>
        val hasOutput = p.hasOutput(compileTo)
        if (!hasOutput) info(s"Source ${p.srcFile}: compilation OK (nothing to emit)")
        hasOutput
      }))
      .flatMap[ValidatedNec[String, Chain[String]]] {
        case Validated.Invalid(e) =>
          Applicative[F].pure(Validated.invalid(e))
        case Validated.Valid(preps) =>
          val backend = targetToBackend(compileTo)
          val results = preps.toList
            .flatMap(p =>
              backend.generate(p.context, bodyConfig).map { compiled =>
                val targetPath = p.targetPath(
                  p.srcFile.getFileName.toString.stripSuffix(".aqua") + compiled.suffix
                )

                targetPath.fold(
                  t => EitherT.leftT[F, Unit](t.getMessage),
                  tp =>
                    FileOps
                      .writeFile(
                        tp,
                        compiled.content
                      )
                      .flatTap { _ =>
                        EitherT.pure(
                          Validated.catchNonFatal(
                            info(
                              s"Result ${tp.toAbsolutePath}: compilation OK (${p.context.funcs.size} functions)"
                            )
                          )
                        )
                      }
                )
              }
            )

          gatherResults(results)
      }
  }

}
