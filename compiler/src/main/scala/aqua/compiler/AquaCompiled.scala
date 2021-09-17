package aqua.compiler

import aqua.backend.Generated

case class AquaCompiled[I](sourceId: I, compiled: Seq[Generated], funcsCount: Int, servicesCount: Int)
