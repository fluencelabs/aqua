package aqua.mangler

import cats.Monoid

import scala.annotation.tailrec

case class ManglerState(lastNumbers: Map[String, Int] = Map.empty) {

  private def genName(name: String, n: Int) =
    s"$name-$n"

  /**
   * Find new unique name, that was never used.
   *
   * @param name        base name
   * @param number      possible suffix for the name
   * @param lastNumbers names with last used numbers
   * @return
   */
  @tailrec
  private def findName(name: String, number: Int, lastNumbers: Map[String, Int]): (String, Int) = {
    val newName = genName(name, number)
    lastNumbers.get(newName) match {
      // if there is such name already - increment number
      case Some(_) =>
        val newNumber = number + 1
        // repeat until find unique name
        findName(name, newNumber, lastNumbers)
      case None => (newName, number)
    }
  }

  // find unique names that have not yet been used
  def findNewNames(introduce: Set[String]): (ManglerState, Map[String, String]) = {
    val (newLastNumbers, newNames) = introduce.foldLeft((lastNumbers, Map.empty[String, String])) {
      case ((accLastNumbers, acc), name) =>
        val (newName, newNumber) = accLastNumbers.get(name) match {
          case Some(n) => findName(name, n + 1, accLastNumbers)
          case None => (name, -1)
        }
        // if number is '-1', then it was not yet in the list, so there is no need to rename it
        val newAcc = if (newNumber == -1) acc else acc + (name -> newName)
        // update last number for introduced name and add new name to a list
        (accLastNumbers + (name -> newNumber) + (newName -> -1), newAcc)
    }

    (copy(lastNumbers = newLastNumbers), newNames)
  }

  // add names to used list
  def forbid(names: Set[String]): ManglerState = {
    val newLastNumbers = names.map(n => n -> lastNumbers.getOrElse(n, -1)).toMap
    copy(lastNumbers = newLastNumbers ++ lastNumbers)
  }

  // forbid name and return unique
  def forbidAndRename(name: String): (ManglerState, String) = {
    val set = Set(name)
    val (newState, newNames) = forbid(set).findNewNames(set)
    (newState, newNames(name))
  }
}

object ManglerState {

  given Monoid[ManglerState] with {
    override val empty: ManglerState = ManglerState()

    override def combine(x: ManglerState, y: ManglerState): ManglerState =
      ManglerState(lastNumbers = x.lastNumbers ++ y.lastNumbers)
  }
}
