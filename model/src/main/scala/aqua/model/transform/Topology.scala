package aqua.model.transform

import aqua.model.body.{CallServiceTag, FuncOp, OnTag, OpTag}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.apply._

object Topology {
  type Tree = Cofree[Chain, OpTag]

  def resolve(op: Tree): Tree =
    transformWithPath(op) {
      case (path, c: CallServiceTag) =>
        Cofree[Chain, OpTag](
          c.copy(peerId = path.collectFirst { case OnTag(peerId, _) =>
            peerId
          }),
          Eval.now(Chain.empty)
        )
      case (_, OnTag(pid, via)) if via.nonEmpty =>
        Cofree[Chain, OpTag](
          OnTag(pid, Nil),
          Eval.now(
            Chain.fromSeq(
              via.map(FuncOp.noop).map(_.tree)
            )
          )
        )
      case (_, t) =>
        Cofree[Chain, OpTag](t, Eval.now(Chain.empty))
    }

  def transformWithPath(cf: Tree, path: List[OpTag] = Nil)(
    f: (List[OpTag], OpTag) => Tree
  ): Tree = {
    val newCf = f(path, cf.head)
    Cofree[Chain, OpTag](
      newCf.head,
      (newCf.tail, cf.tail)
        .mapN(_ ++ _)
        // IF make foldLeft here, will be possible to get info from prev sibling
        .map(_.map(transformWithPath(_, newCf.head :: path)(f)))
    )
  }
}
