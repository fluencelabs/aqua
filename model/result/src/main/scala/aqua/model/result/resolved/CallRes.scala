package aqua.model.result.resolved

import aqua.model.ValueModel
import aqua.model.func.Call

case class CallRes(args: List[ValueModel], exportTo: Option[Call.Export])
