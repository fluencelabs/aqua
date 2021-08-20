package aqua.backend.air

import aqua.backend.{Backend, Generated}
import aqua.model.transform.res.AquaRes
import cats.syntax.show.*

object AirBackend extends Backend {

  val ext = ".air"

  override def generate(aqua: AquaRes): Seq[Generated] = {
    aqua.funcs.toList
      .map(fr => Generated("." + fr.funcName + ext, FuncAirGen(fr).generate.show))
  }
}
