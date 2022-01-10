package aqua.raw.arrow

import aqua.raw.value.ValueRaw
import aqua.raw.Raw

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends Raw {

  def capture(
    arrows: Map[String, Func],
    constants: Map[String, ValueRaw]
  ): Func =
    Func(name, arrow.body.fixXorPar, arrow.`type`, arrow.ret, arrows, constants)

}
