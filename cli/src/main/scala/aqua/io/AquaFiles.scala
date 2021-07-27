package aqua.io

import aqua.files.FileModuleId
import aqua.linker.Modules
import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.Monad
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.applicative._

import java.nio.file.Path

object AquaFiles {
  type Mods[T] = Modules[FileModuleId, AquaFileError, T => T]
  type ETC[F[_], T] = EitherT[F, NonEmptyChain[AquaFileError], T]

  def readSources[F[_]: AquaIO: Monad](
    sourcePath: Path
  ): ETC[F, Chain[AquaFile]] =
    EitherT(
      AquaIO[F]
        .listAqua(sourcePath)
        .flatMap[ValidatedNec[AquaFileError, Chain[AquaFile]]] {
          case Validated.Invalid(e) =>
            Validated.invalid[NonEmptyChain[AquaFileError], Chain[AquaFile]](e).pure[F]
          case Validated.Valid(paths) =>
            paths
              .traverse(AquaFile.read(_))
              .leftMap(NonEmptyChain.one)
              .value
              .map(Validated.fromEither)
        }
        .map(_.toEither)
    )

  def createModules[F[_]: AquaIO: Monad, T](
    sources: Chain[AquaFile],
    importFromPaths: List[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    sources
      .map(_.createModule(transpile, importFromPaths))
      .foldLeft[ETC[F, Mods[T]]](
        EitherT.rightT(Modules())
      ) { case (modulesF, modF) =>
        for {
          ms <- modulesF
          m <- modF
        } yield ms.add(m, export = true)
      }

  def resolveModules[F[_]: AquaIO: Monad, T](
    modules: Modules[FileModuleId, AquaFileError, T => T],
    importFromPaths: List[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    modules.dependsOn.map { case (moduleId, unresolvedErrors) =>
      AquaFile
        .read[F](moduleId.file)
        .leftMap(unresolvedErrors.prepend)
        .flatMap(_.createModule(transpile, importFromPaths))

    }.foldLeft[ETC[F, Mods[T]]](
      EitherT.rightT(modules)
    ) { case (modulesF, modF) =>
      for {
        ms <- modulesF
        m <- modF
      } yield ms.add(m)
    }.flatMap {
      case ms if ms.isResolved =>
        EitherT.rightT(ms)
      case ms => resolveModules(ms, importFromPaths, transpile)
    }

  def readAndResolve[F[_]: AquaIO: Monad, T](
    sourcePath: Path,
    importFromPaths: List[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    for {
      sources <- readSources(sourcePath)
      sourceModules <- createModules(sources, importFromPaths, transpile)
      resolvedModules <- resolveModules(sourceModules, importFromPaths, transpile)
    } yield resolvedModules

}
