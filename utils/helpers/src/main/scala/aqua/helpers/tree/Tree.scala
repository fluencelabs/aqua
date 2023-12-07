package aqua.helpers.tree

import cats.free.Cofree
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.{Eval, Show, Traverse}

object Tree {

  def show[F[_]: Traverse, A: Show](
    what: Cofree[F, A]
  ): String =
    Cofree
      .cata[F, A, List[String]](what) { case (head, tail) =>
        Eval.later {
          val children = tail.combineAll.map("| " + _)
          val parent = head.show

          if (children.isEmpty) List(parent)
          else (parent + ":") +: children
        }
      }
      .value
      .mkString("\n")

}
