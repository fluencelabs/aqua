package aqua.model

import cats.data.Chain

case class ScriptModel(funcs: Chain[FuncModel]) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.append(f))
    case _ => this
  }

  def resolveFunctions: Chain[FuncResolved] =
    funcs
      .foldLeft((Map.empty[String, FuncCallable], Chain.empty[FuncResolved])) {
        case ((funcsAcc, outputAcc), func) =>
          val fr = func.captureArrows(funcsAcc)
          funcsAcc.updated(func.name, fr) -> outputAcc.append(FuncResolved(func.name, fr))
      }
      ._2

}
