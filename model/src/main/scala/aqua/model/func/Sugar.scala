package aqua.model.func

import aqua.model.ValueModel
import aqua.raw.ops.{Call, FlattenTag, FuncOp, ParTag, SeqTag}
import aqua.raw.value.ValueRaw
import cats.data.{Chain, State}
import cats.syntax.traverse.*
import cats.instances.list.*

object Sugar {

  def desugarize[S: Counter](value: ValueRaw): State[S, (ValueRaw, Option[FuncOp])] =
    for {
      i <- Counter[S].get
      (vm, map) = ValueModel.recursiveRaw(value, i)
      _ <- Counter[S].add(map.size)

      ops <- map.toList.traverse { case (name, v) =>
        desugarize(v).map {
          case (vv, Some(op)) =>
            FuncOp.node(SeqTag, Chain(op, FuncOp.leaf(FlattenTag(v, name))))

          case _ =>
            FuncOp.leaf(FlattenTag(v, name))
        }
      }

      // Take values from a chain
      // for each, recursiveRaw
      // group all into par
      // for each, recursiveRaw
      // if anything returned, put it into seq before this
    } yield (
      vm.toRaw,
      ops match {
        case Nil => None
        case x :: Nil => Option(x)
        case _ => Option(FuncOp.node(ParTag, Chain.fromSeq(ops)))
      }
    )

  def desugarize[S: Counter](
    values: List[ValueRaw]
  ): State[S, List[(ValueRaw, Option[FuncOp])]] =
    values.traverse(desugarize(_))

  def desugarize[S: Counter](call: Call): State[S, (Call, Option[FuncOp])] =
    desugarize(call.args).map(list =>
      call.copy(list.map(_._1)) -> (list.flatMap(_._2) match {
        case Nil => None
        case x :: Nil => Option(x)
        case vs => Option(FuncOp.node(ParTag, Chain.fromSeq(vs)))
      })
    )
}
