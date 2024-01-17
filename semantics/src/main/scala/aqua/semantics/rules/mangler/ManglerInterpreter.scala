package aqua.semantics.rules.mangler

import cats.data.State
import monocle.Lens

class ManglerInterpreter[X](using
  lens: Lens[X, ManglerState]
) extends ManglerAlgebra[State[X, *]] {

  def genName(name: String, n: Int) = s"$name-$n"

  override def rename(name: String): State[X, String] =
    for {
      s <- get
      newNameAndNum = s.forbidden.get(name).map(n => genName(name, n + 1) -> (n + 1)).getOrElse(genName(name, 0) -> 0)
      (newName, newNum) = newNameAndNum
      _ = println("newName = " + newName)
      _ = println("newNum = " + newNum)
      _ <- modify(st => st.copy(forbidden = st.forbidden + (name -> newNum)))
    } yield newName

  private lazy val get: State[X, ManglerState] =
    State.get[X].map(lens.get)

  private def modify(f: ManglerState => ManglerState): State[X, Unit] =
    State.modify[X](lens.modify(f))
}
