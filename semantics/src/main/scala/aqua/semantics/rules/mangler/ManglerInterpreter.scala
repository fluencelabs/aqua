package aqua.semantics.rules.mangler

import aqua.mangler.ManglerState

import cats.data.State
import monocle.Lens

class ManglerInterpreter[X](using
  lens: Lens[X, ManglerState]
) extends ManglerAlgebra[State[X, *]] {

  override def rename(name: String): State[X, String] =
    apply(_.forbidAndRename(name))

  private def apply[A](f: ManglerState => (ManglerState, A)): State[X, A] =
    State.apply(lens.modifyF(f andThen (_.swap)) andThen (_.swap))
}
