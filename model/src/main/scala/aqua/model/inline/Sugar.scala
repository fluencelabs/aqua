package aqua.model.inline

import aqua.model.inline.state.Counter
import aqua.model.{IntoFieldModel, IntoIndexModel, LambdaModel, LiteralModel, ValueModel, VarModel}
import aqua.raw.ops.*
import aqua.raw.value.{IntoFieldRaw, IntoIndexRaw, LambdaRaw, LiteralRaw, ValueRaw, VarRaw}
import cats.data.{Chain, State}
import cats.syntax.traverse.*
import cats.instances.list.*

object Sugar {

  // Todo: use state monad instead of counter
  private def unfold(raw: ValueRaw, i: Int): (ValueModel, Map[String, ValueRaw]) = raw match {
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

  // Todo: use state monad instead of counter
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

  private def parDesugarPrefix(ops: List[FuncOp]): Option[FuncOp] = ops match {
    case Nil => None
    case x :: Nil => Option(x)
    case _ => Option(FuncOp.node(ParTag, Chain.fromSeq(ops)))
  }

  private def parDesugarPrefixOpt(ops: Option[FuncOp]*): Option[FuncOp] =
    parDesugarPrefix(ops.toList.flatten)

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
    } yield vm.toRaw -> parDesugarPrefix(ops)

  def desugarize[S: Counter](
                              values: List[ValueRaw]
                            ): State[S, List[(ValueRaw, Option[FuncOp])]] =
    values.traverse(desugarize(_))

  def desugarize[S: Counter](call: Call): State[S, (Call, Option[FuncOp])] =
    desugarize(call.args).map(list =>
      call.copy(list.map(_._1)) -> parDesugarPrefix(list.flatMap(_._2))
    )

  // TODO: here we should return smth in between Raw and Res (model?)
  def desugarize[S: Counter](tag: RawTag): State[S, (RawTag, Option[FuncOp])] =
    tag match {
      case OnTag(peerId, via) =>
        for {
          peerIdDe <- desugarize(peerId)
          viaDe <- desugarize(via.toList)
          (pid, pif) = peerIdDe
          viaD = Chain.fromSeq(viaDe.map(_._1))
          viaF = viaDe.flatMap(_._2)

        } yield OnTag(pid, viaD) -> parDesugarPrefix(viaF.prependedAll(pif))

      case MatchMismatchTag(left, right, shouldMatch) =>
        for {
          ld <- desugarize(left)
          rd <- desugarize(right)
        } yield MatchMismatchTag(ld._1, rd._1, shouldMatch) -> parDesugarPrefixOpt(ld._2, rd._2)

      case ForTag(item, iterable) =>
        desugarize(iterable).map { case (v, p) =>
          ForTag(item, v) -> p
        }

      case CallArrowTag(funcName, call) =>
        desugarize(call).map {
          case (c, p) => CallArrowTag(funcName, c) -> p
        }

      case CallServiceTag(serviceId, funcName, call) =>
        for {
          cd <- desugarize(call)
          sd <- desugarize(serviceId)
        } yield CallServiceTag(sd._1, funcName, cd._1) -> parDesugarPrefixOpt(sd._2, cd._2)

      case CanonicalizeTag(operand, exportTo) =>
        desugarize(operand).map { case (v, p) =>
          CanonicalizeTag(v, exportTo) -> p
        }

      // TODO: it should not appear as a Tag, only as Res
      case FlattenTag(operand, assignTo) =>
        desugarize(operand).map { case (v, p) =>
          FlattenTag(v, assignTo) -> p
        }

      case JoinTag(operands) =>
        operands.traverse(desugarize).map(nel =>
          JoinTag(nel.map(_._1)) -> parDesugarPrefix(nel.toList.flatMap(_._2))
        )

      case _ => State.pure(tag -> None)
    }
}
