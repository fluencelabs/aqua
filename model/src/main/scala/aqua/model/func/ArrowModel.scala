package aqua.model.func

import aqua.model.func.raw.FuncOp
import aqua.types.ArrowType
import aqua.model.{Model, ValueModel}

case class ArrowModel(
  `type`: ArrowType,
  ret: List[ValueModel],
  body: FuncOp
) extends Model
