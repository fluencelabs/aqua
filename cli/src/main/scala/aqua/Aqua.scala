package aqua

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypescriptFile
import aqua.model.{Model, ScriptModel}
import aqua.parser.Ast
import cats.data.ValidatedNec
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import aqua.semantics.Semantics
import cats.syntax.show._

object Aqua {

  def parseString(input: String): ValidatedNec[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input).leftMap(_.map(pe => SyntaxError(pe.failedAtOffset, pe.expected)))

  def parseFileString(name: String, input: String): ValidatedNec[AquaError, Ast[FileSpan.F]] = {
    implicit val fileLift: LiftParser[FileSpan.F] = FileSpan.fileSpanLiftParser(name, input)
    Ast
      .fromString[FileSpan.F](input)
      .leftMap(_.map(pe => SyntaxError(pe.failedAtOffset, pe.expected)))
  }

  // Will fail if imports are used
  def generateModel(input: String): ValidatedNec[AquaError, Model] =
    parseString(input).andThen(ast =>
      Semantics.generateModel(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2)))
    )

  def generate(input: String, air: Boolean): ValidatedNec[AquaError, String] =
    generateModel(input).map {
      case m: ScriptModel if air =>
        // TODO it's meaningless to compile all functions to air, as resulting .air file is incorrect; only one function should be taken
        m.resolveFunctions
          .map(FuncAirGen)
          .map(g =>
            // add function name before body
            s";; function name: ${g.func.funcName}\n\n" + g.generateAir.show
          )
          .toList
          .mkString("\n\n\n")

      case m: ScriptModel =>
        TypescriptFile(m).generateTS()

      case _ => "//No input given"
    }

}
