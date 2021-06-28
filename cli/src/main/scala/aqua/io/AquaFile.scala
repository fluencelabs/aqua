package aqua.io

import aqua.io.AquaFiles.ETC
import aqua.linker.AquaModule
import aqua.parser.head.ImportExpr
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import aqua.parser.{Ast, BlockIndentError, FuncReturnError, LexerError}
import aqua.{CustomSyntaxError, SyntaxError}
import cats.Eval
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.Concurrent
import cats.parse.LocationMap
import cats.syntax.apply._
import cats.syntax.functor._
import fs2.io.file.Files

import java.nio.file.{Path, Paths}
import scala.collection.immutable

case class AquaFile(
  id: FileModuleId,
  imports: Map[String, FileSpan.Focus],
  source: String,
  ast: Ast[FileSpan.F]
) {

  /**
   * Gathers all errors and results
   */
  private def gatherResolvedResults[F[_]: Concurrent](
    results: immutable.Iterable[EitherT[F, AquaFileError, (FileModuleId, FileNotFound)]]
  ): ETC[F, Map[FileModuleId, AquaFileError]] = {
    results
      .foldLeft[AquaFiles.ETC[F, Map[FileModuleId, AquaFileError]]](EitherT.rightT(Map())) {
        case (files, nextFile) =>
          EitherT((files.value, nextFile.value).mapN {
            case (files, Right(resolvedImport)) =>
              files.map(_ + resolvedImport)
            case (Right(_), Left(err)) =>
              Left(NonEmptyChain(err))
            case (Left(errs), Left(err)) =>
              Left(errs.append(err))
          })
      }
  }

  def createModule[F[_]: Concurrent, T](
    transpile: Ast[FileSpan.F] => T => T,
    importFrom: List[Path]
  ): AquaFiles.ETC[F, AquaModule[FileModuleId, AquaFileError, T]] = {
    val resolvedImports = imports.map { case (pathString, focus) =>
      FileModuleId
        .resolve(focus, Paths.get(pathString), id.file.getParent +: importFrom)
        .map(id => (id -> FileNotFound(focus, id.file, importFrom)))
    }

    for {
      importsWithInfo <- gatherResolvedResults(resolvedImports)
    } yield AquaModule(
      id,
      importsWithInfo,
      transpile(ast)
    )
  }
}

object AquaFile {

  def parseAst(name: String, input: String): Either[AquaFileError, Ast[F]] = {
    implicit val fileLift: LiftParser[FileSpan.F] = FileSpan.fileSpanLiftParser(name, input)
    Ast
      .fromString[FileSpan.F](input)
      .leftMap(_.map {
        case BlockIndentError(indent, message) => CustomSyntaxError(indent._1, message)
        case FuncReturnError(point, message) => CustomSyntaxError(point._1, message)
        case LexerError(pe) =>
          val fileSpan =
            FileSpan(
              name,
              input,
              Eval.later(LocationMap(input)),
              Span(pe.failedAtOffset, pe.failedAtOffset + 1)
            )
          SyntaxError(fileSpan, pe.expected)
      })
      .toEither
      .left
      .map(AquaScriptErrors(_))
  }

  def read[F[_]: Files: Concurrent](file: Path): EitherT[F, AquaFileError, AquaFile] = {
    for {
      sourceOp <- EitherT.right(
        FileOps
          .readSourceText[F](file)
          .map {
            _.left
              .map(t => FileSystemError(t))
          }
          .compile
          .last
      )
      source <- EitherT.fromEither(sourceOp.getOrElse(Left(EmptyFileError(file))))
      _ <- EitherT.fromEither(
        if (source.isEmpty) Left(EmptyFileError(file): AquaFileError) else Right(())
      )
      ast <- EitherT.fromEither(parseAst(file.toString, source))
      imports = ast.head.tailForced
        .map(_.head)
        .collect { case ImportExpr(filename) =>
          val path = filename.value.drop(1).dropRight(1)
          val focus = filename.unit._1.focus(1)
          path -> focus
        }
        .collect { case (path, Some(focus)) =>
          path -> focus
        }
        .toList
        .toMap
    } yield {
      AquaFile(
        FileModuleId(file.toAbsolutePath.normalize()),
        imports,
        source,
        ast
      )
    }
  }
}
