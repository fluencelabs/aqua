package aqua.linker

case class AquaModule[I, E, T](id: I, dependsOn: Map[I, E], body: T => T)
