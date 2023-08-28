package aqua.model.inline.state

import cats.data.State

trait Mangler[S] {
  self =>
  def getForbiddenNames: State[S, Set[String]]

  def findNewNames(introduce: Set[String]): State[S, Map[String, String]]

  def findNewName(introduce: String): State[S, String] =
    findNewNames(Set(introduce)).map(_.getOrElse(introduce, introduce))

  def findAndForbidName(introduce: String): State[S, String] =
    for {
      n <- findNewName(introduce)
      _ <- forbid(Set(n))
    } yield n

  def findAndForbidNames(introduce: Set[String]): State[S, Map[String, String]] =
    for {
      n <- findNewNames(introduce)
      _ <- forbid(n.values.toSet)
    } yield n

  def forbid(names: Set[String]): State[S, Unit]

  def forbidName(name: String): State[S, Unit] =
    forbid(Set(name))

  def transformS[R](f: R => S, g: (R, S) => R): Mangler[R] =
    new Mangler[R] {

      val getForbiddenNames: State[R, Set[String]] =
        self.getForbiddenNames.transformS(f, g)

      def findNewNames(introduce: Set[String]): State[R, Map[String, String]] =
        self.findNewNames(introduce).transformS(f, g)

      def forbid(names: Set[String]): State[R, Unit] =
        self.forbid(names).transformS(f, g)
    }
}

object Mangler {
  def apply[S](implicit mangler: Mangler[S]): Mangler[S] = mangler

  implicit object Simple extends Mangler[Set[String]] {
    val getForbiddenNames: State[Set[String], Set[String]] = State.get

    def findNewNames(introduce: Set[String]): State[Set[String], Map[String, String]] =
      getForbiddenNames.map(forbidden =>
        (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
          acc + (name -> LazyList
            .from(0)
            .map(name + "-" + _)
            .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
            .head)
        }
      )

    def forbid(names: Set[String]): State[Set[String], Unit] =
      State.modify(_ ++ names)
  }
}
