package aqua

import aqua.parser.lift.{FileSpan, LiftParser, Span}
import aqua.parser.{Ast, BlockIndentError, FuncReturnError, LexerError}
import cats.Eval
import cats.data.ValidatedNec
import cats.parse.LocationMap

object Aqua {

  def parseFileString(name: String, input: String): ValidatedNec[AquaError, Ast[FileSpan.F]] = {
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
  }

}
