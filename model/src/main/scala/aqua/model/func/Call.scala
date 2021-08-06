package aqua.model.func

import aqua.model.{ValueModel, VarModel}
import aqua.types.Type

case class Call(args: List[ValueModel], exportTo: List[Call.Export]) {

  def mapValues(f: ValueModel => ValueModel): Call =
    Call(
      args.map(f),
      exportTo
    )

  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(_.mapName(f)))

  def argVarNames: Set[String] = args.collect { case VarModel(name, _, _) =>
    name
  }.toSet

  override def toString: String =
    s"[${args.mkString(" ")}]${exportTo.map(_.model).map(" " + _).mkString(",")}"
}

object Call {

  case class Export(name: String, `type`: Type) {
    def mapName(f: String => String): Export = copy(f(name))

    def model: ValueModel = VarModel(name, `type`)
  }
}
