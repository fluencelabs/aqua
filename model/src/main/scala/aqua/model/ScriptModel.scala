package aqua.model

import cats.data.Chain
import cats.syntax.show._

case class ScriptModel(funcs: Chain[FuncModel]) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.append(f))
    case _ => this
  }

  def generateModel: String =
    funcs
      .foldLeft((Map.empty[String, FuncCallable], Chain.empty[String])) {
        case ((funcsAcc, outputAcc), func) =>
          val fr = func.captureArrows(funcsAcc).value
          funcsAcc.updated(func.name, fr) -> outputAcc.append(fr.toString)
      }
      ._2
      .toList
      .mkString("\n\n")

}
