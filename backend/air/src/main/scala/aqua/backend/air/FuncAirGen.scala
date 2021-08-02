package aqua.backend.air

import aqua.model.func.FuncCallable
import aqua.model.transform.{GenerationConfig, Transform}

case class FuncAirGen(func: FuncCallable) {

  /**
   * Generates AIR from the function body
   */
  def generateAir(conf: GenerationConfig = GenerationConfig()): Air =
    AirGen(
      Transform.forClient(func, conf)
    ).generate
}
