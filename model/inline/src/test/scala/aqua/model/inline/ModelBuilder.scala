package aqua.model.inline

import aqua.model.{CallServiceModel, ValueModel, VarModel}

object ModelBuilder {

  def add(l: ValueModel, r: ValueModel)(o: VarModel): CallServiceModel =
    CallServiceModel(
      "math",
      "add",
      List(l, r),
      o
    )
}
