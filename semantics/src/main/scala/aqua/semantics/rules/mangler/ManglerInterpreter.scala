package aqua.semantics.rules.mangler

import cats.data.State
import monocle.Lens
import monocle.macros.GenLens

class ManglerInterpreter[X](using
  lens: Lens[X, ManglerState]
) extends ManglerAlgebra[State[X, *]] {

  override def rename(name: String): State[X, String] =
    for {
      s <- get
      newName = LazyList
        .from(0)
        .map(i => s"$name-$i")
        .dropWhile(s.isForbidden)
        .head
      _ <- modify(_.forbid(newName))
    } yield newName

  private lazy val get: State[X, ManglerState] =
    State.get[X].map(lens.get)

  private def modify(f: ManglerState => ManglerState): State[X, Unit] =
    State.modify[X](lens.modify(f))
}
