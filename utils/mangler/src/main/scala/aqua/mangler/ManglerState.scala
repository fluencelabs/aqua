package aqua.mangler

import cats.Monoid

case class ManglerState(namesNumbers: Map[String, Int] = Map.empty) {

  private def genName(name: String, n: Int) =
    s"$name-$n"

  // find unique names that have not yet been used
  def findNewNames(introduce: Set[String]): (ManglerState, Map[String, String]) = {
    introduce.foldLeft(this, Map.empty[String, String]) { case ((state, newNames), name) =>
      val namesNumbers = state.namesNumbers
      if (!namesNumbers.contains(name)) {
        val newState = state.copy(
          namesNumbers = namesNumbers
            .updated(name, 0)
        )

        (newState, newNames)
      } else {
        val (newNumber, newName) = LazyList
          .from(namesNumbers.getOrElse(name, 0))
          .map(n => n -> genName(name, n))
          .dropWhile { case (_, newName) =>
            namesNumbers.contains(newName)
          }
          .head
        val newState = copy(
          namesNumbers = namesNumbers
            .updated(name, newNumber + 1)
            .updated(newName, 0)
        )

        (newState, newNames + (name -> newName))
      }
    }
  }

  // add names to used list
  def forbid(names: Set[String]): ManglerState = {
    val newLastNumbers = names.map(n => n -> namesNumbers.getOrElse(n, 0)).toMap
    copy(namesNumbers = newLastNumbers ++ namesNumbers)
  }

  // forbid name and return unique
  def forbidAndRename(name: String): (ManglerState, String) = {
    val set = Set(name)
    val (newState, newNames) = forbid(set).findNewNames(set)
    (newState, newNames(name))
  }
}
