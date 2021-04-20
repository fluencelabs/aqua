package aqua

import aqua.parser.Ast
import cats.data.ValidatedNec
import aqua.parser.lift.{FileSpan, LiftParser, Span}

object Aqua {

  def parseString(input: String): ValidatedNec[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input).leftMap(_.map(pe => SyntaxError(pe.failedAtOffset, pe.expected)))

  def parseFileString(name: String, input: String): ValidatedNec[AquaError, Ast[FileSpan.F]] = {
    implicit val fileLift: LiftParser[FileSpan.F] = FileSpan.fileSpanLiftParser(name, input)
    Ast
      .fromString[FileSpan.F](input)
      .leftMap(_.map(pe => SyntaxError(pe.failedAtOffset, pe.expected)))
  }

}
