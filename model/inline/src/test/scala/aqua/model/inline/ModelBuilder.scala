package aqua.model.inline

import aqua.model.{CallModel, CallServiceModel, LiteralModel, ValueModel, VarModel}

object ModelBuilder {

  def add(l: ValueModel, r: ValueModel)(o: VarModel): CallServiceModel =
    CallServiceModel(
      serviceId = "math",
      funcName = "add",
      args = List(l, r),
      result = o
    )
}
