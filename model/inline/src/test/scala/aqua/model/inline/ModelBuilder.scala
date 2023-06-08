package aqua.model.inline

import aqua.model.{CallModel, CallServiceModel, LiteralModel, ValueModel, VarModel}

object ModelBuilder {

  def add(l: ValueModel, r: ValueModel)(o: VarModel): CallServiceModel =
    CallServiceModel(
      // Have to use direct constructor bc
      // need string with LiteralType.string here
      LiteralModel.liftString("math"),
      "add",
      CallModel(
        args = List(l, r),
        exportTo = List(CallModel.Export(o.name, o.`type`))
      )
    )
}
