package aqua.model

import aqua.raw.ops.Call
import aqua.types.{ArrowType, Type}

// TODO docs
case class CallModel(args: List[ValueModel], exportTo: List[CallModel.Export]) {
  override def toString: String = s"[${args.mkString(" ")}] ${exportTo.mkString(" ")}"

  def arrowArgNames: Set[String] = args.collect { case VarModel(m, _: ArrowType, _) =>
    m
  }.toSet
}

object CallModel {
  case class Export(name: String, `type`: Type)

  def callExport(ex: Call.Export): Export = Export(ex.name, ex.`type`)
}
