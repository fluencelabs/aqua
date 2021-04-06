package aqua

import aqua.backend.{air, ts}
import aqua.backend.air.FuncAirGen
import aqua.backend.ts.{TypescriptFile, TypescriptFunc}
import aqua.model.{FuncCallable, Model, ScriptModel}
import aqua.parser.Ast
import cats.data.{Chain, ValidatedNec}
import aqua.parser.lift.Span
import aqua.semantics.Semantics
import cats.syntax.show._

object Aqua {

  implicit class ScriptGen(m: ScriptModel) {

    def generateAir: String =
      m.funcs
        .foldLeft((Map.empty[String, FuncCallable], Chain.empty[String])) {
          case ((funcsAcc, outputAcc), func) =>
            val fr = func.captureArrows(funcsAcc).value
            funcsAcc.updated(func.name, fr) -> outputAcc.append(
              // add function name before body
              s";; function name: ${func.name}\n\n" + FuncAirGen(func.name, fr).generateAir.show
            )
        }
        ._2
        .toList
        .mkString("\n\n\n")

    def generateTypescript: String =
      TypescriptFile(
        m.funcs
          .foldLeft((Map.empty[String, FuncCallable], Chain.empty[TypescriptFunc])) {
            case ((funcsAcc, outputAcc), func) =>
              val fr = func.captureArrows(funcsAcc).value
              val fag = air.FuncAirGen(func.name, fr)

              def generateTs: TypescriptFunc =
                ts.TypescriptFunc(fag.name, fag.func, fag.generateTsAir)

              funcsAcc.updated(func.name, fr) -> outputAcc.append(generateTs)
          }
          ._2
          .toList
      ).show
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
