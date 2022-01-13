package aqua.raw.arrow

import aqua.raw.value.ValueRaw
import aqua.raw.Raw

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends Raw {

  def capture(
               arrows: Map[String, FuncArrow],
               constants: Map[String, ValueRaw]
  ): FuncArrow =
    FuncArrow(name, arrow.body.fixXorPar, arrow.`type`, arrow.ret, arrows, constants)

}
