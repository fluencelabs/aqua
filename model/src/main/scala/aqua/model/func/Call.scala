package aqua.model.func

import aqua.model.{ValueModel, VarModel}
import aqua.types.Type

case class Call(args: List[ValueModel], exportTo: Option[Call.Export]) {

  def mapValues(f: ValueModel => ValueModel): Call =
    Call(
      args.map(f),
      exportTo
    )

  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(_.mapName(f)))
}

object Call {

  case class Export(name: String, `type`: Type) {
    def mapName(f: String => String): Export = copy(f(name))

    def model: ValueModel = VarModel(name, `type`)
  }
}
