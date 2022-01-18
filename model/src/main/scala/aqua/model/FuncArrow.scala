package aqua.model

import aqua.raw.Raw
import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.FuncOp
import aqua.raw.value.ValueRaw
import aqua.types.{ArrowType, Type}

case class FuncArrow(
                      funcName: String,
                      body: FuncOp,
                      arrowType: ArrowType,
                      ret: List[ValueRaw],
                      capturedArrows: Map[String, FuncArrow],
                      capturedValues: Map[String, ValueModel]
                    ) {

  lazy val args: List[(String, Type)] = arrowType.domain.toLabelledList()
  lazy val argNames: List[String] = args.map(_._1)

}

object FuncArrow {

  def fromRaw(
               raw: FuncRaw,
               arrows: Map[String, FuncArrow],
               constants: Map[String, ValueModel]
             ): FuncArrow =
    FuncArrow(
      raw.name,
      raw.arrow.body.fixXorPar,
      raw.arrow.`type`,
      raw.arrow.ret,
      arrows,
      constants
    )
}
