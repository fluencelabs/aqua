package aqua.model

import aqua.generator.{FuncAirGen, TypescriptFile, TypescriptFunc}
import cats.data.Chain
import cats.syntax.show._

case class ScriptModel(funcs: Chain[FuncModel]) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.append(f))
    case _ => this
  }

  def generateAir: Chain[String] =
    funcs
      .foldLeft((Map.empty[String, FuncCallable], Chain.empty[String])) { case ((funcsAcc, outputAcc), func) =>
        val fr = func.toContext(funcsAcc).value
        funcsAcc.updated(func.name, fr) -> outputAcc.append(FuncAirGen(func.name, fr).generateAir.show)
      }
      ._2

  def generateTypescript: String =
    TypescriptFile(
      funcs
        .foldLeft((Map.empty[String, FuncCallable], Chain.empty[TypescriptFunc])) {
          case ((funcsAcc, outputAcc), func) =>
            val fr = func.toContext(funcsAcc).value
            val fag = FuncAirGen(func.name, fr)
            funcsAcc.updated(func.name, fr) -> outputAcc.append(fag.generateTs)
        }
        ._2
        .toList
    ).show

  def generateModel: String =
    funcs
      .foldLeft((Map.empty[String, FuncCallable], Chain.empty[String])) { case ((funcsAcc, outputAcc), func) =>
        val fr = func.toContext(funcsAcc).value
        funcsAcc.updated(func.name, fr) -> outputAcc.append(fr.toString)
      }
      ._2
      .toList
      .mkString("\n\n")

}
