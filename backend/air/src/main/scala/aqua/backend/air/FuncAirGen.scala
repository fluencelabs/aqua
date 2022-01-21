package aqua.backend.air

import aqua.res.FuncRes

case class FuncAirGen(func: FuncRes) {

  /**
   * Generates AIR from the function body
   */
  def generate: Air =
    AirGen(
      func.body
    ).generate
}
