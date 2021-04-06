package aqua.model.body

import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.apply._

object Topology {

  def resolve(op: FuncOp): FuncOp =
    FuncOp(transformWithPath(op.tree) {
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
    })

  def transformWithPath(cf: Cofree[Chain, OpTag], path: List[OpTag] = Nil)(
    f: (List[OpTag], OpTag) => Cofree[Chain, OpTag]
  ): Cofree[Chain, OpTag] = {
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
