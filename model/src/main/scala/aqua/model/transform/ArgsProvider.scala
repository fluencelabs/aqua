package aqua.model.transform

import aqua.model.func.body.FuncOp

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}
