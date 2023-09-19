package aqua.semantics.rules.mangler

import cats.kernel.Monoid

final case class ManglerState(
  forbidden: Set[String] = Set.empty
) {

  def isForbidden(name: String): Boolean =
    forbidden.contains(name)

  def forbid(name: String): ManglerState =
    copy(forbidden = forbidden + name)
}

object ManglerState {

  given Monoid[ManglerState] with {
    override val empty: ManglerState = ManglerState()

    override def combine(x: ManglerState, y: ManglerState): ManglerState =
      ManglerState(forbidden = x.forbidden ++ y.forbidden)
  }
}
