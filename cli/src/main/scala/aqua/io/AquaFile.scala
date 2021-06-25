package aqua.io

import aqua.Aqua
import aqua.linker.AquaModule
import aqua.parser.Ast
import aqua.parser.head.ImportExpr
import aqua.parser.lift.FileSpan
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.Concurrent
import cats.syntax.apply._
import cats.syntax.functor._
import fs2.io.file.Files

import java.nio.file.{Path, Paths}

case class AquaFile(
  id: FileModuleId,
  imports: Map[String, FileSpan.Focus],
  source: String,
  ast: Ast[FileSpan.F]
) {

  def module[F[_]: Concurrent, T](
    transpile: Ast[FileSpan.F] => T => T,
    importFrom: LazyList[Path]
  ): AquaFiles.ETC[F, AquaModule[FileModuleId, AquaFileError, T]] =
    imports.map { case (k, v) =>
      FileModuleId.resolve(v, Paths.get(k), id.file.getParent +: importFrom).map(_ -> v)
    }.foldLeft[AquaFiles.ETC[F, AquaModule[FileModuleId, AquaFileError, T]]](
      EitherT.rightT(
        AquaModule(
          id,
          Map(),
          transpile(ast)
        )
      )
    ) { case (modF, nextF) =>
      EitherT((modF.value, nextF.value).mapN {
        case (moduleV, Right(dependency)) =>
          moduleV.map(m =>
            m.copy(dependsOn =
              m.dependsOn + dependency.map(FileNotFound(_, dependency._1.file, importFrom))
            )
          )
        case (Right(_), Left(err)) =>
          Left(NonEmptyChain(err))
        case (Left(errs), Left(err)) =>
          Left(errs.append(err))
      })
    }

}

object AquaFile {

  def readAst[F[_]: Files: Concurrent](
    file: Path
  ): fs2.Stream[F, Either[AquaFileError, (String, Ast[FileSpan.F])]] =
    FileOps
      .readSourceText[F](file)
      .map {
        _.left
          .map(t => FileSystemError(t))
      }
      .map(
        _.flatMap(source =>
          Aqua
            .parseFileString(file.toString, source)
            .map(source -> _)
            .toEither
            .left
            .map(AquaScriptErrors(_))
        )
      )

  def read[F[_]: Files: Concurrent](file: Path): EitherT[F, AquaFileError, AquaFile] =
    EitherT(readAst[F](file).compile.last.map(_.getOrElse(Left(EmptyFileError(file))))).map {
      case (source, ast) =>
        AquaFile(
          FileModuleId(file.toAbsolutePath.normalize()),
          ast.head.tailForced
            .map(_.head)
            .collect { case ImportExpr(filename) =>
              val fn = filename.value.drop(1).dropRight(1)
              val focus = filename.unit._1.focus(1)
              fn -> focus
            }
            .collect { case (a, Some(b)) =>
              a -> b
            }
            .toList
            .toMap,
          source,
          ast
        )
    }

}
