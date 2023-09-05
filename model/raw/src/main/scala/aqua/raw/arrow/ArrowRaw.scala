package aqua.raw.arrow

import aqua.raw.ops.RawTag
import aqua.types.ArrowType
import aqua.raw.Raw
import aqua.raw.value.ValueRaw

case class ArrowRaw(
  `type`: ArrowType,
  ret: List[ValueRaw],
  body: RawTag.Tree
) extends Raw

// func -internal-():
       // AssignemntTag(res1, CallArrowRaw("serviceId"))
//     res1 <- CallArrowRaw("serviceId")
//     <- res1
