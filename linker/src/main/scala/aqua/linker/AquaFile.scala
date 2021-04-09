package aqua.linker

import aqua.parser.Ast
import aqua.parser.head.ImportExpr
import aqua.parser.lift.FileSpan
import cats.data.EitherT
import cats.syntax.apply._

import java.io.File

case class AquaFile(
  file: File,
  imports: Set[String],
  ast: Ast[FileSpan.F]
) {

  def resolve(name: String, value: AquaFile): AquaFile = if (imports(name))
    copy(
      imports = imports - name,
      ast = ast.copy(tree = ast.tree.copy(tail = (value.ast.tree.tail, ast.tree.tail).mapN(_ ++ _)))
    )
  else this

  def isResolved: Boolean = imports.isEmpty
}

object AquaFile {

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
    )
}
