package aqua.raw.arrow

import aqua.raw.Raw
import aqua.raw.ops.FuncOp
import aqua.raw.value.ValueRaw
import aqua.types.{ArrowType, Type}

case class FuncArrow(
                      funcName: String,
                      body: FuncOp,
                      arrowType: ArrowType,
                      ret: List[ValueRaw],
                      capturedArrows: Map[String, FuncArrow],
                      capturedValues: Map[String, ValueRaw]
) extends Raw {

  lazy val args: List[(String, Type)] = arrowType.domain.toLabelledList()
  lazy val argNames: List[String] = args.map(_._1)

}
