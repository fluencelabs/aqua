package aqua.backend.air

import aqua.backend.{Backend, Compiled}
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.implicits.toShow

object AirBackend extends Backend {

  val ext = ".air"

  override def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled] = {
    context.funcs.values.toList.map(fc =>
      Compiled("." + fc.funcName + ext, FuncAirGen(fc).generateAir(bc).show)
    )
  }
}
