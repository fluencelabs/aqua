package aqua.model.func.raw

import cats.Show
import cats.free.Cofree

object ShowFuncOp {

  private def showTreeOffset(offset: Int): Show[FuncOp.Tree] = { case Cofree(head, tail) =>
    val children = tail.value
    s"${" " * offset}$head" +
      (if (children.isEmpty) "\n"
       else
         " {\n" + children.toList
           .map(showTreeOffset(offset + 1).show) + s"${" " * offset}}\n")
  }

  implicit val showFuncOp: Show[FuncOp] =
    Show.show(op => showTreeOffset(0).show(op.tree))
}
