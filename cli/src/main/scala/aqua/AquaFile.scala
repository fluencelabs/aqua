package aqua

import aqua.cli.{CliError, EmptyFileError}
import aqua.parser.Ast
import aqua.parser.head.ImportExpr
import aqua.parser.lift.FileSpan
import cats.data.{Chain, EitherT}
import cats.effect.Concurrent
import fs2.io.file.Files
import fs2.text
import cats.syntax.functor._

import java.io.File

case class AquaFile(file: File, imports: Chain[String], ast: Ast[FileSpan.F])

object AquaFile {

  def readSourceText[F[_]: Files: Concurrent](file: File): fs2.Stream[F, Either[CliError, String]] =
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
  ): fs2.Stream[F, Either[CliError, Ast[FileSpan.F]]] =
    readSourceText[F](file).map(
      _.flatMap(source =>
        Aqua
          .parseFileString(file.getName, source)
          .toEither
          .left
          .map(CliError.errorInfo(file.getName, source, _))
      )
    )

  def read[F[_]: Files: Concurrent](file: File): EitherT[F, CliError, AquaFile] =
    EitherT(readAst[F](file).compile.last.map(_.getOrElse(Left(EmptyFileError(file))))).map(ast =>
      AquaFile(
        file,
        ast.head.tailForced.map(_.head).collect { case ImportExpr(filename) =>
          filename.value.drop(1).dropRight(1)
        },
        ast
      )
    )
}
