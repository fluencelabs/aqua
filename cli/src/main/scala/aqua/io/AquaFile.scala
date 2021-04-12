package aqua.io

import aqua.Aqua
import aqua.linker.{Module, Modules}
import aqua.parser.Ast
import aqua.parser.head.ImportExpr
import aqua.parser.lift.FileSpan
import cats.data.EitherT
import cats.effect.Concurrent
import fs2.io.file.Files
import fs2.text
import cats.syntax.functor._

import java.nio.file.Path

case class AquaFile(
  id: FileModuleId,
  imports: Map[String, FileSpan.Focus],
  source: String,
  ast: Ast[FileSpan.F]
) {

  def register[T](
    modules: Modules[FileModuleId, AquaFileError, T],
    transpile: Ast[FileSpan.F] => T => T,
    export: Boolean = false
  ): Modules[FileModuleId, AquaFileError, T] = ???
  //modules.add(Module(id, imports.view.mapValues(f => ), transpile(ast)), export)

}

object AquaFile {

  def readSourceText[F[_]: Files: Concurrent](
    file: Path
  ): fs2.Stream[F, Either[AquaFileError, String]] =
    Files[F]
      .readAll(file, 4096)
      .through(text.utf8Decode)
      .attempt
      .map {
        _.left
          .map(t => FileSystemError(t))
      }

  def readAst[F[_]: Files: Concurrent](
    file: Path
  ): fs2.Stream[F, Either[AquaFileError, (String, Ast[FileSpan.F])]] =
    readSourceText[F](file).map(
      _.flatMap(source =>
        Aqua
          .parseFileString(file.toString, source)
          .map(source -> _)
          .toEither
          .left
          .map(AquaScriptErrors(file.toString, source, _))
      )
    )

  def read[F[_]: Files: Concurrent](file: Path): EitherT[F, AquaFileError, AquaFile] =
    EitherT(readAst[F](file).compile.last.map(_.getOrElse(Left(EmptyFileError(file))))).map {
      case (source, ast) =>
        AquaFile(
          FileModuleId(file.toAbsolutePath),
          ast.head.tailForced
            .map(_.head)
            .collect { case ImportExpr(filename) =>
              val fn = filename.value.drop(1).dropRight(1)
              val focus = filename.unit._1.focus(source, 1)
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
