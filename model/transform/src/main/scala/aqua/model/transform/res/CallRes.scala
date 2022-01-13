package aqua.model.transform.res

import aqua.model.ValueModel
import aqua.model.CallModel

// TODO docs
case class CallRes(args: List[ValueModel], exportTo: Option[CallModel.Export]) {
  override def toString: String = s"[${args.mkString(" ")}]${exportTo.fold("")(" " + _)}"
}
