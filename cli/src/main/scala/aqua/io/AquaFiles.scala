package aqua.io

import aqua.linker.Modules
import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.effect.kernel.Concurrent
import cats.syntax.apply._
import fs2.io.file.Files

import java.io.File
import java.nio.file.Path

case class AquaFiles(
  output: List[String],
  fetched: Map[String, AquaFile],
  unresolvedImports: Set[String],
  source: File,
  importFrom: List[File]
) {}

object AquaFiles {
  type Mods[T] = Modules[FileModuleId, AquaFileError, T]
  type ETC[F[_], T] = EitherT[F, NonEmptyChain[AquaFileError], T]

  def readSources[F[_]: Files: Concurrent](
    sourcePath: Path
  ): ETC[F, Chain[AquaFile]] =
    // TODO wrap this with F, as it could fail
    sourcePath.toFile
      .listFiles()
      .toList
      .map {
        case f if f.isFile && f.getName.endsWith(".aqua") =>
          println("coing to parse " + f)
          AquaFile
            .read(f.toPath.toAbsolutePath)
            .map(Chain(_))
            .leftMap(NonEmptyChain.one)

        case f if f.isDirectory =>
          readSources(f.toPath)
      }
      .foldLeft[ETC[F, Chain[AquaFile]]](
        EitherT.rightT(Chain.empty)
      ) { case (accF, nextF) =>
        EitherT((accF.value, nextF.value).mapN {
          case (Right(acc), Right(v)) => Right(acc ++ v)
          case (Left(acc), Left(v)) => Left(acc ++ v)
          case (Left(acc), _) => Left(acc)
          case (_, Left(v)) => Left(v)
        })
      }

  def sourceModules[F[_]: Concurrent, T](
    sources: Chain[AquaFile],
    importFromPaths: LazyList[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    sources
      .map(_.module(transpile, importFromPaths))
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
    importFromPaths: LazyList[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    modules.dependsOn.map { case (moduleId, unresolvedErrors) =>
      AquaFile
        .read[F](moduleId.file)
        .leftMap(unresolvedErrors.prepend)
        .flatMap(_.module(transpile, importFromPaths))

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
    importFromPaths: LazyList[Path],
    transpile: Ast[FileSpan.F] => T => T
  ): ETC[F, Mods[T]] =
    for {
      srcs <- readSources(sourcePath)
      srcMods <- sourceModules(srcs, importFromPaths, transpile)
      resMods <- resolveModules(srcMods, importFromPaths, transpile)
    } yield resMods

}
