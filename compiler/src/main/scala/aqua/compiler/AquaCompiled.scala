package aqua.compiler

import aqua.backend.Compiled

case class AquaCompiled[I](sourceId: I, compiled: Seq[Compiled])
