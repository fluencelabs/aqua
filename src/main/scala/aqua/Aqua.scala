package aqua

import aqua.ast.gen.{Gen, ScriptGen}
import aqua.ast.{Ast, Compiler}
import cats.data.ValidatedNel
import aqua.parser.lift.Span

import scala.collection.immutable.Queue

object Aqua {

  def parse(input: String): ValidatedNel[AquaError, Ast[Span.F]] =
    Ast.fromString[Span.F](input)

  def compile(input: String): ValidatedNel[AquaError, Gen] =
    parse(input).andThen(ast => Compiler.compile(ast).leftMap(_.map(ts => CompilerError(ts._1.unit._1, ts._2))))

  def generate(input: String): ValidatedNel[AquaError, Queue[String]] =
    compile(input).map {
      case g: ScriptGen => g.generateAir
      case _ => Queue.empty
    }
}
