package aqua.model.func.body

import cats.Show
import cats.free.Cofree

object ShowFuncOp {

  private def showTreeOffset(offset: Int): Show[FuncOp.Tree] = { case Cofree(head, tail) =>
    val children = tail.value
    s"${" ".repeat(offset)}$head" +
      (if (children.isEmpty) "\n"
       else
         " {\n" + children.toList
           .map(showTreeOffset(offset + 1).show) + s"${" ".repeat(offset)}}\n")
  }

  implicit val showFuncOp: Show[FuncOp] =
    Show.show(op => showTreeOffset(0).show(op.tree))
}
