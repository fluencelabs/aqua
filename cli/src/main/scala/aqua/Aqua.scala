package aqua

import aqua.parser.{Ast, BlockIndentError, FuncReturnError, LexerError}
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import cats.data.ValidatedNec

object Aqua {

  def parseString(input: String): ValidatedNec[AquaError, Ast[Span.F]] =
    Ast
      .fromString[Span.F](input)
      .leftMap(_.map {
        case BlockIndentError(indent, message) => CustomSyntaxError(indent._1, message)
        case FuncReturnError(point, message) => CustomSyntaxError(point._1, message)
        case LexerError(pe) => SyntaxError(pe.failedAtOffset, pe.expected)
      })

  def parseFileString(name: String, input: String): ValidatedNec[AquaError, Ast[FileSpan.F]] = {
    implicit val fileLift: LiftParser[FileSpan.F] = FileSpan.fileSpanLiftParser(name, input)
    Ast
      .fromString[FileSpan.F](input)
      .leftMap(_.map {
        case BlockIndentError(indent, message) => CustomSyntaxError(indent._1.span, message)
        case FuncReturnError(point, message) => CustomSyntaxError(point._1.span, message)
        case LexerError(pe) => SyntaxError(pe.failedAtOffset, pe.expected)
      })
  }

}
