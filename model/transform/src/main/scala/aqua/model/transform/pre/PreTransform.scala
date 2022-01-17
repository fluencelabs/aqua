package aqua.model.transform.pre

import aqua.raw.ops.FuncOp

trait PreTransform {
  def transform(op: FuncOp): FuncOp
}
