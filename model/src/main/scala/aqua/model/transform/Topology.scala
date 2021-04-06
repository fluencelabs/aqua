package aqua.model.transform

import aqua.model.body.{CallServiceTag, FuncOp, OnTag, OpTag}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.apply._

object Topology {
  type Tree = Cofree[Chain, OpTag]

  // TODO: after topology is resolved, OnTag should be eliminated
  def resolve(op: Tree): Tree =
    transformWithPath(op) {
      case (path, c: CallServiceTag) if c.peerId.isEmpty =>
        Cofree[Chain, OpTag](
          c.copy(peerId = path.collectFirst { case OnTag(peerId, _) =>
            peerId
          }),
          Eval.now(Chain.empty)
        )
      case (path, tag @ OnTag(pid, via)) =>
        // Drop seq/par/xor from path
        val pathOn = path.collect { case ot: OnTag =>
          ot
        }

        pathOn match {
          // If we are on the right node, do nothing
          case Nil =>
            Cofree[Chain, OpTag](tag, Eval.now(Chain.empty))
          case h :: _ if h.peerId == pid =>
            Cofree[Chain, OpTag](tag, Eval.now(Chain.empty))
          case h :: _ =>
            Cofree[Chain, OpTag](
              tag,
              Eval.now(
                Chain.fromSeq(
                  (h.via.reverse ++ via).map(FuncOp.noop).map(_.tree)
                )
              )
            )
        }

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
