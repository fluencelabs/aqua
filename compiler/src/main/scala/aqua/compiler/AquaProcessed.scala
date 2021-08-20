package aqua.compiler

import aqua.model.AquaContext

case class AquaProcessed[I](id: I, context: AquaContext) {
  def hasOutput: Boolean = context.funcs.nonEmpty || context.services.nonEmpty
}
