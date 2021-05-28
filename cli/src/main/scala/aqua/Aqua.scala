package aqua

import aqua.parser.lift.{FileSpan, LiftParser}
import aqua.parser.{Ast, BlockIndentError, FuncReturnError, LexerError}
import cats.data.ValidatedNec

object Aqua {

  def parseFileString(name: String, input: String): ValidatedNec[AquaError, Ast[FileSpan.F]] = {
    implicit val fileLift: LiftParser[FileSpan.F] = FileSpan.fileSpanLiftParser(name, input)
    Ast
      .fromString[FileSpan.F](input)
      .leftMap(_.map {
        case BlockIndentError(indent, message) => CustomSyntaxError(name, indent._1.span, message)
        case FuncReturnError(point, message) => CustomSyntaxError(name, point._1.span, message)
        case LexerError(pe) => SyntaxError(name, pe.failedAtOffset, pe.expected)
      })
  }

}
