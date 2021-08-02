package aqua.backend.air

import aqua.backend.{Backend, Generated}
import aqua.model.AquaContext
import aqua.model.transform.GenerationConfig
import cats.implicits.toShow

object AirBackend extends Backend {

  val ext = ".air"

  override def generate(context: AquaContext, genConf: GenerationConfig): Seq[Generated] = {
    context.funcs.values.toList.map(fc =>
      Generated("." + fc.funcName + ext, FuncAirGen(fc).generateAir(genConf).show)
    )
  }
}
