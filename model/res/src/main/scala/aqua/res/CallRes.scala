package aqua.res

import aqua.model.{CallModel, ValueModel}

// TODO docs
case class CallRes(args: List[ValueModel], exportTo: Option[CallModel.Export]) {
  override def toString: String = s"[${args.mkString(" ")}]${exportTo.fold("")(" " + _)}"
}
