package aqua.backend.air

import aqua.model.func.FuncResolved
import aqua.model.transform.BodyConfig

case class FuncAirGen(func: FuncResolved) {

  /**
   * Generates AIR from the function body as it is, with no modifications and optimizations
   */
  def generateAir: Air =
    AirGen(func.func.body.tree).generate

  /**
   * Generates AIR from the optimized function body, assuming client is behind a relay
   * @return
   */
  def generateClientAir(conf: BodyConfig = BodyConfig()): Air =
    AirGen(
      func.forClient(conf)
    ).generate
}
