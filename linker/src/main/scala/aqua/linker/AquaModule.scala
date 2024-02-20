package aqua.linker

case class AquaModule[I, E, T](id: I, imports: Map[String, I], dependsOn: Map[I, E], body: T)
