package aqua.backend.air

import aqua.model.func.FuncCallable
import aqua.model.transform.{BodyConfig, Transform}

case class FuncAirGen(func: FuncCallable) {

  /**
   * Generates AIR from the function body as it is, with no modifications and optimizations
   */
  def generateAir: Air =
    AirGen(func.body.tree).generate

  /**
   * Generates AIR from the optimized function body, assuming client is behind a relay
   * @return
   */
  def generateClientAir(conf: BodyConfig = BodyConfig()): Air =
    AirGen(
      Transform.forClient(func, conf)
    ).generate
}
