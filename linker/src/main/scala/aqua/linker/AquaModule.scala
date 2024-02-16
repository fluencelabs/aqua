package aqua.linker

case class AquaModule[I, E, T](id: I, imports: Map[String, I], dependsOn: Map[I, E], body: T) {
  def map[TT](f: (T, AquaModule[I, E, T]) => TT): AquaModule[I, E, TT] = copy(body = f(body, this))

  def mapWithId[TT](f: (I, T) => TT): AquaModule[I, E, TT] = copy(body = f(id, body))

  def mapErr[EE](f: E => EE): AquaModule[I, EE, T] =
    copy(dependsOn = dependsOn.view.mapValues(f).toMap)
}
