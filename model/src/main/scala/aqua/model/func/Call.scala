package aqua.model.func

import aqua.model.ValueModel
import aqua.types.Type

case class Call(args: List[Call.Arg], exportTo: Option[String]) {

  def mapValues(f: ValueModel => ValueModel): Call =
    Call(
      args.map(_.mapValues(f)),
      exportTo
    )

  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(f))
}

object Call {

  case class Arg(model: ValueModel, `type`: Type) {

    def mapValues(f: ValueModel => ValueModel): Arg =
      copy(f(model))
  }
}
