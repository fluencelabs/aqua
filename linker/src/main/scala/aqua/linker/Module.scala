package aqua.linker

case class Module[I, E, T](id: I, dependsOn: Map[I, E], body: T => T)
