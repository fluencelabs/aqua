package aqua.model

import aqua.generator.{ArrowCallable, FuncAirGen, TypescriptFile, TypescriptFunc}
import cats.data.Chain
import cats.syntax.show._

case class ScriptModel(funcs: Chain[FuncModel]) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.append(f))
    case _ => this
  }

  def generateAir: Chain[String] =
    funcs
      .foldLeft((Map.empty[String, ArrowCallable], Chain.empty[String])) { case ((funcsAcc, outputAcc), func) =>
        val fag = FuncAirGen(func)
        funcsAcc.updated(func.name, fag.callable) -> outputAcc.append(fag.generateAir(funcsAcc).show)
      }
      ._2

  def generateTypescript: String =
    TypescriptFile(
      funcs
        .foldLeft((Map.empty[String, ArrowCallable], Chain.empty[TypescriptFunc])) {
          case ((funcsAcc, outputAcc), func) =>
            val fag = FuncAirGen(func)
            funcsAcc.updated(func.name, fag.callable) -> outputAcc.append(fag.generateTs(funcsAcc))
        }
        ._2
        .toList
    ).show

}
