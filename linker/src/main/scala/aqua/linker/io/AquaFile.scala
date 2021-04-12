package aqua.linker.io

import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.syntax.apply._

import java.io.File

case class AquaFile(
  id: FileModuleId,
  imports: Map[FileModuleId, LinkerError],
  ast: Ast[FileSpan.F]
) {}

object AquaFile {
  /*
  def readSourceText[F[_]: Files: Concurrent](
    file: File
  ): fs2.Stream[F, Either[LinkerError, String]] =
    Files[F]
      .readAll(file.toPath, 4096)
      .through(text.utf8Decode)
      .attempt
      .map {
        _.left
          .map(t => CliError.ioError("Error on reading file", t))
      }

  def readAst[F[_]: Files: Concurrent](
    file: File
  ): fs2.Stream[F, Either[LinkerError, Ast[FileSpan.F]]] =
    readSourceText[F](file).map(
      _.flatMap(source =>
        Aqua
          .parseFileString(file.getName, source)
          .toEither
          .left
          .map(CliError.errorInfo(file.getName, source, _))
      )
    )

  def read[F[_]: Files: Concurrent](file: File): EitherT[F, LinkerError, AquaFile] =
    EitherT(readAst[F](file).compile.last.map(_.getOrElse(Left(EmptyFileError(file))))).map(ast =>
      AquaFile(
        file,
        ast.head.tailForced
          .map(_.head)
          .collect { case ImportExpr(filename) =>
            filename.value.drop(1).dropRight(1)
          }
          .toList
          .toSet,
        ast
      )
    )*/
}
