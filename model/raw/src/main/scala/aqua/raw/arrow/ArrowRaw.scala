package aqua.raw.arrow

import aqua.raw.ops.FuncOp
import aqua.types.ArrowType
import aqua.raw.Raw
import aqua.raw.value.ValueRaw

case class ArrowRaw(
  `type`: ArrowType,
  ret: List[ValueRaw],
  body: FuncOp
) extends Raw
