package aqua

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypescriptFile
import aqua.model.{Model, ScriptModel}
import aqua.parser.Ast
import cats.data.ValidatedNec
import aqua.parser.lift.Span
import aqua.semantics.Semantics
import cats.syntax.show._

object Aqua {

  implicit class ScriptGen(m: ScriptModel) {

    def generateAir: String =
      m.resolveFunctions
        .map(FuncAirGen)
        .map(g =>
          // add function name before body
          s";; function name: ${g.func.name}\n\n" + g.generateAir.show
        )
        .toList
        .mkString("\n\n\n")

    def generateTypescript: String =
      TypescriptFile(m).show
  }

  def parse(input: String): ValidatedNec[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input).leftMap(_.map(pe => SyntaxError(pe.failedAtOffset, pe.expected)))

  def generateModel(input: String): ValidatedNec[AquaError, Model] =
    parse(input).andThen(ast =>
      Semantics.generateModel(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2)))
    )

  def generate(input: String, air: Boolean): ValidatedNec[AquaError, String] =
    generateModel(input).map {
      case m: ScriptModel =>
        if (air) m.generateAir else m.generateTypescript
      case _ => "//No input given"
    }

  def generateTS(input: String): ValidatedNec[AquaError, String] =
    generate(input, air = false)

  def generateAir(input: String): ValidatedNec[AquaError, String] =
    generate(input, air = true)
}
