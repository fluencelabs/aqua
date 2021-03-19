package aqua

import aqua.generator.{Gen, ScriptGen}
import aqua.parser.Ast
import cats.data.ValidatedNel
import aqua.parser.lift.Span
import aqua.semantics.Semantics

import scala.collection.immutable.Queue

object Aqua {

  def parse(input: String): ValidatedNel[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input)

  def validate(input: String): ValidatedNel[AquaError, Gen] =
    parse(input).andThen(ast => Semantics.validate(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2))))

  def generate(input: String): ValidatedNel[AquaError, Queue[String]] =
    validate(input).map {
      case g: ScriptGen => g.generateAir
      case _ => Queue.empty
    }
}
