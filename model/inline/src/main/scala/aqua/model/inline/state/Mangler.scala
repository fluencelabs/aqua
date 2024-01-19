package aqua.model.inline.state

import cats.data.State
import aqua.mangler.ManglerState

trait Mangler[S] {
  self =>
  def getForbiddenNames: State[S, ManglerState]

  def findAndForbidName(introduce: String): State[S, String] =
    findAndForbidNames(Set(introduce)).map(_.getOrElse(introduce, introduce))

  def findAndForbidNames(introduce: Set[String]): State[S, Map[String, String]]

  def forbid(names: Set[String]): State[S, Unit]

  def transformS[R](f: R => S, g: (R, S) => R): Mangler[R] =
    new Mangler[R] {

      val getForbiddenNames: State[R, ManglerState] =
        self.getForbiddenNames.transformS(f, g)

      def forbid(names: Set[String]): State[R, Unit] =
        self.forbid(names).transformS(f, g)

      def findAndForbidNames(introduce: Set[String]): State[R, Map[String, String]] =
        self.findAndForbidNames(introduce).transformS(f, g)
    }
}

object Mangler {
  def apply[S](implicit mangler: Mangler[S]): Mangler[S] = mangler

  implicit object Simple extends Mangler[ManglerState] {
    val getForbiddenNames: State[ManglerState, ManglerState] = State.get

    def findAndForbidNames(introduce: Set[String]): State[ManglerState, Map[String, String]] =
      getForbiddenNames.flatMap(forbidden =>
        val (newState, newNames) = forbidden.findNewNames(introduce)
        State.modify[ManglerState](_ => newState).map(_ => newNames)
      )

    def forbid(names: Set[String]): State[ManglerState, Unit] =
      State.modify(st => st.forbid(names))
  }
}
