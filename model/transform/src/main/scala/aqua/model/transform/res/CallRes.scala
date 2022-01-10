package aqua.model.transform.res

import aqua.model.ValueModel
import aqua.raw.ops.Call

// TODO docs
case class CallRes(args: List[ValueModel], exportTo: Option[Call.Export]) {
  override def toString: String = s"[${args.mkString(" ")}]${exportTo.fold("")(" " + _)}"
}
