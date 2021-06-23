package aqua.backend

import aqua.model.AquaContext
import aqua.model.transform.BodyConfig

case class Compiled(suffix: String, content: String)

trait Backend {
  def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled]
}
