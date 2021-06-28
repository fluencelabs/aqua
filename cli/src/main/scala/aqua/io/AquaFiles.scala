package aqua.io

import aqua.linker.Modules
import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.effect.kernel.Concurrent
import cats.syntax.apply._
import fs2.io.file.Files

import java.nio.file.Path
import scala.util.Try

object AquaFiles {
  type Mods[T] = Modules[FileModuleId, AquaFileError, T]
  type ETC[F[_], T] = EitherT[F, NonEmptyChain[AquaFileError], T]

  def readSources[F[_]: Files: Concurrent](
    sourcePath: Path
  ): ETC[F, Chain[AquaFile]] =
    // TODO use effect instead of Try
    EitherT
      .fromEither[F](
        Try {
          val f = sourcePath.toFile
          if (f.isDirectory) {
            f.listFiles().toList
          } else {
            List(f)
          }
        }.toEither
      )
      .leftMap[AquaFileError](FileSystemError)
      .leftMap(NonEmptyChain.one)
      .flatMap(
        _.collect {
          case f if f.isFile && f.getName.endsWith(".aqua") =>
            AquaFile
              .read(f.toPath.toAbsolutePath.normalize())
              .map(Chain(_))
              .leftMap(NonEmptyChain.one)
          case f if f.isDirectory =>
            readSources(f.toPath)
        }
          .foldLeft[ETC[F, Chain[AquaFile]]](
            EitherT.rightT(Chain.empty)
          ) { case (accF, nextF) =>
            EitherT((accF.value, nextF.value).mapN {
              case (Right(acc), Right(v)) =>
                Right(acc ++ v)
              case (Left(acc), Left(v)) =>
                Left(acc ++ v)
              case (Left(acc), _) =>
                Left(acc)
              case (_, Left(v)) =>
                Left(v)
            })
          }
      )

  def createModules[F[_]: Concurrent, T](
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

  def resolveModules[F[_]: Files: Concurrent, T](
    modules: Modules[FileModuleId, AquaFileError, T],
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

  def readAndResolve[F[_]: Files: Concurrent, T](
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
