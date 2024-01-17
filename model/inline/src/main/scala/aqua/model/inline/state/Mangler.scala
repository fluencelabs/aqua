package aqua.model.inline.state

import cats.data.State

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

case class ManglerState(lastNumbers: Map[String, Int] = Map.empty)

object Mangler {
  def apply[S](implicit mangler: Mangler[S]): Mangler[S] = mangler

  private def genName(name: String, n: Int) =
    s"$name-$n"

  implicit object Simple extends Mangler[ManglerState] {
    val getForbiddenNames: State[ManglerState, ManglerState] = State.get

    def findAndForbidNames(introduce: Set[String]): State[ManglerState, Map[String, String]] =
      getForbiddenNames.flatMap(forbidden =>
        val (newLastNumbers, newNames) =
          introduce.foldLeft((forbidden.lastNumbers, Map.empty[String, String])) {
            case ((lastNumbers, acc), name) =>
              val (newName, newNumber) =
                lastNumbers.get(name).map(n => (genName(name, n), n + 1)).getOrElse((name, 0))
              (lastNumbers + (name -> newNumber), acc + (name -> newName))
          }
        State.modify[ManglerState](st => st.copy(lastNumbers = newLastNumbers)).map(_ => newNames)
      )

    def forbid(names: Set[String]): State[ManglerState, Unit] =
      State.modify(st =>
        st.copy(lastNumbers = st.lastNumbers ++ names.map(n => n -> st.lastNumbers.getOrElse(n, 0)))
      )
  }
}
