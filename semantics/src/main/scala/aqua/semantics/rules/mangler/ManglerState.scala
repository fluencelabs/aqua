package aqua.semantics.rules.mangler

import cats.kernel.Monoid

final case class ManglerState(
  forbidden: Map[String, Int] = Map.empty
)

object ManglerState {

  given Monoid[ManglerState] with {
    override val empty: ManglerState = ManglerState()

    override def combine(x: ManglerState, y: ManglerState): ManglerState =
      ManglerState(forbidden = x.forbidden ++ y.forbidden)
  }
}
