package aqua.model.inline

import aqua.model.inline.state.Counter
import aqua.model.{IntoFieldModel, IntoIndexModel, LambdaModel, LiteralModel, ValueModel, VarModel}
import aqua.raw.ops.{Call, FlattenTag, FuncOp, ParTag, SeqTag}
import aqua.raw.value.{IntoFieldRaw, IntoIndexRaw, LambdaRaw, LiteralRaw, ValueRaw, VarRaw}
import cats.data.{Chain, State}
import cats.syntax.traverse.*
import cats.instances.list.*

object Sugar {
  private
  def unfold(raw: ValueRaw, i: Int): (ValueModel, Map[String, ValueRaw]) = raw match {
    case VarRaw(name, t, lambda) if lambda.isEmpty =>
      VarModel(name, t, Chain.empty) -> Map.empty
    case LiteralRaw(value, t) =>
      LiteralModel(value, t) -> Map.empty
    case VarRaw(name, t, lambda) =>
      val (lambdaModel, map) =
        lambda.foldLeft(Chain.empty[LambdaModel] -> Map.empty[String, ValueRaw]) {
          case ((lc, m), l) =>
            val (lm, mm) = unfoldLambda(l, i + m.size)
            (lc :+ lm, m ++ mm)
        }
      VarModel(name, t, lambdaModel) -> map
  }

  private def unfoldLambda(l: LambdaRaw, i: Int): (LambdaModel, Map[String, ValueRaw]) = l match {
    case IntoFieldRaw(field, t) => IntoFieldModel(field, t) -> Map.empty
    case IntoIndexRaw(vm@VarRaw(name, _, l), t) if l.nonEmpty =>
      val ni = name + "-" + i
      IntoIndexModel(ni, t) -> Map(ni -> vm)
    case IntoIndexRaw(VarRaw(name, _, _), t) =>
      IntoIndexModel(name, t) -> Map.empty

    case IntoIndexRaw(LiteralRaw(value, _), t) =>
      IntoIndexModel(value, t) -> Map.empty
  }

  def desugarize[S: Counter](value: ValueRaw): State[S, (ValueRaw, Option[FuncOp])] =
    for {
      i <- Counter[S].get
      (vm, map) = unfold(value, i)
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
