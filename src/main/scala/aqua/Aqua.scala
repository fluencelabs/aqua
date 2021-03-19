package aqua

import aqua.model.{Model, ScriptModel}
import aqua.parser.Ast
import cats.data.ValidatedNel
import aqua.parser.lift.Span
import aqua.semantics.Semantics

import scala.collection.immutable.Queue

object Aqua {

  def parse(input: String): ValidatedNel[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input)

  def validate(input: String): ValidatedNel[AquaError, Model] =
    parse(input).andThen(ast => Semantics.validate(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2))))

  def generate(input: String): ValidatedNel[AquaError, String] =
    validate(input).map {
      case g: ScriptModel => g.generateTypescript
      case _ => "//No input given"
    }
}
