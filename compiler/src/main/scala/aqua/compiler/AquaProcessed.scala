package aqua.compiler

import aqua.model.AquaContext

case class AquaProcessed[I](id: I, context: AquaContext)
