package aqua.semantics.rules.mangler

import aqua.mangler.ManglerState

import cats.data.State
import monocle.Lens

class ManglerInterpreter[X](using
  lens: Lens[X, ManglerState]
) extends ManglerAlgebra[State[X, *]] {

  override def rename(name: String): State[X, String] =
    for {
      state <- get
      result = state.forbidAndRename(name)
      (newState, newName) = result
      _ <- modify(_ => newState)
    } yield newName

  private lazy val get: State[X, ManglerState] =
    State.get[X].map(lens.get)

  private def modify(f: ManglerState => ManglerState): State[X, Unit] =
    State.modify[X](lens.modify(f))
}
