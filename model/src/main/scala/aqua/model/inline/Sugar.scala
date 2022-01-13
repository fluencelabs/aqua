package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.{CallModel, CallServiceModel, CanonicalizeModel, FlattenModel, ForModel, IntoFieldModel, IntoIndexModel, JoinModel, LambdaModel, LiteralModel, MatchMismatchModel, OnModel, OpModel, ParModel, PushToStreamModel, SeqModel, ValueModel, VarModel, XorModel}
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

  private def parDesugarPrefix(ops: List[OpModel.Tree]): Option[OpModel.Tree] = ops match {
    case Nil => None
    case x :: Nil => Option(x)
    case _ => Option(OpModel.par(ops))
  }

  private def parDesugarPrefixOpt(ops: Option[OpModel.Tree]*): Option[OpModel.Tree] =
    parDesugarPrefix(ops.toList.flatten)

  def desugarize[S: Counter](value: ValueRaw): State[S, (ValueModel, Option[OpModel.Tree])] =
    for {
      i <- Counter[S].get
      (vm, map) = unfold(value, i)
      _ <- Counter[S].add(map.size)

      ops <- map.toList.traverse { case (name, v) =>
        desugarize(v).map {
          case (vv, Some(op)) =>
            OpModel.seq(op :: FlattenModel(vv, name).leaf :: Nil)

          case (vv, _) =>
            FlattenModel(vv, name).leaf
        }
      }
    } yield vm -> parDesugarPrefix(ops)

  def desugarize[S: Counter](
                              values: List[ValueRaw]
                            ): State[S, List[(ValueModel, Option[OpModel.Tree])]] =
    values.traverse(desugarize(_))

  def desugarize[S: Counter](
                              call: Call
                            ): State[S, (CallModel, Option[OpModel.Tree])] =
    desugarize(call.args).map(list =>
      (
        CallModel(
          list.map(_._1),
          call.exportTo.map(CallModel.callExport)
        ),
        parDesugarPrefix(list.flatMap(_._2))
      )
    )

  private def pure[S](op: OpModel): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(Some(op -> None))

  private def none[S]: State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(None)

  def desugarize[S: Counter : Mangler : Arrows : Exports](tag: RawTag): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    tag match {
      case OnTag(peerId, via) =>
        for {
          peerIdDe <- desugarize(peerId)
          viaDe <- desugarize(via.toList)
          (pid, pif) = peerIdDe
          viaD = Chain.fromSeq(viaDe.map(_._1))
          viaF = viaDe.flatMap(_._2)

        } yield Some(OnModel(pid, viaD) -> parDesugarPrefix(viaF.prependedAll(pif)))

      case MatchMismatchTag(left, right, shouldMatch) =>
        for {
          ld <- desugarize(left)
          rd <- desugarize(right)
        } yield Some(MatchMismatchModel(ld._1, rd._1, shouldMatch) -> parDesugarPrefixOpt(ld._2, rd._2))

      case ForTag(item, iterable) =>
        desugarize(iterable).map { case (v, p) =>
          Some(ForModel(item, v) -> p)
        }

      case PushToStreamTag(operand, exportTo) =>
        desugarize(operand).map {
          case (v, p) =>
            Some(PushToStreamModel(v, exportTo.name) -> p)
        }

      case CallServiceTag(serviceId, funcName, call) =>
        for {
          cd <- desugarize(call)
          sd <- desugarize(serviceId)
        } yield Some(CallServiceModel(sd._1, funcName, cd._1) -> parDesugarPrefixOpt(sd._2, cd._2))

      case CanonicalizeTag(operand, exportTo) =>
        desugarize(operand).map { case (v, p) =>
          Some(CanonicalizeModel(v, CallModel.callExport(exportTo)) -> p)
        }

      case JoinTag(operands) =>
        operands
          .traverse(desugarize)
          .map(nel => Some(JoinModel(nel.map(_._1)) -> parDesugarPrefix(nel.toList.flatMap(_._2))))

      case CallArrowTag(funcName, call) =>
        Arrows[S].arrows.flatMap(_.get(funcName) match {
          case Some(fn) =>
            desugarize(call).flatMap{
              case (cm, p) =>
                ArrowInliner.callArrow(fn, cm).map(body => )
            }
        })
        for {
          arrows <- Arrows[S].arrows
        }desugarize(call)

      case SeqTag => pure(SeqModel)
      case _: ParGroupTag => pure(ParModel)
      case XorTag => pure(XorModel)
      case _: NoExecTag => none
    }
}
