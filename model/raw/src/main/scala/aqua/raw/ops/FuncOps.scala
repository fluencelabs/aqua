package aqua.raw.ops

import aqua.raw.value.{LiteralRaw, ValueRaw}
import cats.data.Chain
import cats.free.Cofree

object FuncOps {

  def seq(ops: FuncOp*): FuncOp =
    if (ops.length == 1) ops.head
    else
      SeqTag
        .wrap(
          ops.flatMap {
            case FuncOp(Cofree(SeqTag, subOps)) => subOps.value.toList
            case FuncOp(cof) => cof :: Nil
          }: _*
        )
        .toFuncOp

}
