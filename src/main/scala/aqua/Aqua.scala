package aqua

import aqua.model.{Model, ScriptModel}
import aqua.parser.Ast
import cats.data.ValidatedNec
import aqua.parser.lift.Span
import aqua.semantics.Semantics

object Aqua {

  def parse(input: String): ValidatedNec[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input)

  def validate(input: String): ValidatedNec[AquaError, Model] =
    parse(input).andThen(ast => Semantics.validate(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2))))

  def generate(input: String, air: Boolean): ValidatedNec[AquaError, String] =
    validate(input).map {
      case g: ScriptModel =>
        if (air) g.generateAir else g.generateTypescript
      case _ => "//No input given"
    }

  def generateTS(input: String): ValidatedNec[AquaError, String] =
    generate(input, air = false)

  def generateAir(input: String): ValidatedNec[AquaError, String] =
    generate(input, air = true)
}
