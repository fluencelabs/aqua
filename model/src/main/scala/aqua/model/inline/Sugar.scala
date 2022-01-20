package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.{
  CallModel,
  CallServiceModel,
  CanonicalizeModel,
  DetachModel,
  EmptyModel,
  FlattenModel,
  ForModel,
  IntoFieldModel,
  IntoIndexModel,
  JoinModel,
  LambdaModel,
  LiteralModel,
  MatchMismatchModel,
  NextModel,
  OnModel,
  OpModel,
  ParModel,
  PushToStreamModel,
  RestrictionModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.raw.ops.*
import aqua.raw.value.{IntoFieldRaw, IntoIndexRaw, LambdaRaw, LiteralRaw, ValueRaw, VarRaw}
import cats.data.{Chain, State}
import cats.syntax.traverse.*
import cats.instances.list.*
import cats.syntax.functor.*
import scribe.{log, Logging}

object Sugar extends Logging {

  private def unfold[S: Counter: Exports](
    raw: ValueRaw
  ): State[S, (ValueModel, Map[String, ValueRaw])] =
    Exports[S].exports.flatMap(exports =>
      raw match {
        case VarRaw(name, t, lambda) if lambda.isEmpty =>
          val vm = VarModel(name, t, Chain.empty).resolveWith(exports)
          State.pure(vm -> Map.empty)
        case LiteralRaw(value, t) =>
          State.pure(LiteralModel(value, t) -> Map.empty)
        case VarRaw(name, t, lambda) =>
          lambda
            .foldLeft[State[S, (Chain[LambdaModel], Map[String, ValueRaw])]](
              State.pure(Chain.empty[LambdaModel] -> Map.empty[String, ValueRaw])
            ) { case (lcm, l) =>
              lcm.flatMap { case (lc, m) =>
                unfoldLambda(l).map { case (lm, mm) =>
                  (lc :+ lm, m ++ mm)
                }
              }
            }
            .map { case (lambdaModel, map) =>
              val vm = VarModel(name, t, lambdaModel).resolveWith(exports)
              vm -> Map.empty
            }
      }
    )

  private def unfoldLambda[S: Counter](
    l: LambdaRaw
  ): State[S, (LambdaModel, Map[String, ValueRaw])] =
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Map.empty)
      case IntoIndexRaw(vm @ VarRaw(name, _, l), t) if l.nonEmpty =>
        Counter[S].incr.map { i =>
          val ni = name + "-" + i
          IntoIndexModel(ni, t) -> Map(ni -> vm)
        }
      case IntoIndexRaw(VarRaw(name, _, _), t) =>
        State.pure(IntoIndexModel(name, t) -> Map.empty)

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Map.empty)
    }

  private def parDesugarPrefix(ops: List[OpModel.Tree]): Option[OpModel.Tree] = ops match {
    case Nil => None
    case x :: Nil => Option(x)
    case _ => Option(ParModel.wrapIfNonEmpty(ops: _*))
  }

  private def parDesugarPrefixOpt(ops: Option[OpModel.Tree]*): Option[OpModel.Tree] =
    parDesugarPrefix(ops.toList.flatten)

  def desugarize[S: Counter: Exports](
    value: ValueRaw
  ): State[S, (ValueModel, Option[OpModel.Tree])] =
    for {
      vmp <- unfold(value)
      (vm, map) = vmp

      ops <- map.toList.traverse { case (name, v) =>
        desugarize(v).map {
          case (vv, Some(op)) =>
            SeqModel.wrapIfNonEmpty(op, FlattenModel(vv, name).leaf)

          case (vv, _) =>
            FlattenModel(vv, name).leaf
        }
      }
    } yield vm -> parDesugarPrefix(ops)

  def desugarize[S: Counter: Exports](
    values: List[ValueRaw]
  ): State[S, List[(ValueModel, Option[OpModel.Tree])]] =
    values.traverse(desugarize(_))

  def desugarize[S: Counter: Exports](
    call: Call
  ): State[S, (CallModel, Option[OpModel.Tree])] =
    desugarize(call.args).map { list =>
      (
        CallModel(
          list.map(_._1),
          call.exportTo.map(CallModel.callExport)
        ),
        parDesugarPrefix(list.flatMap(_._2))
      )
    }

  private def pure[S](op: OpModel): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(Some(op -> None))

  private def none[S]: State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(None)

  def desugarize[S: Counter: Mangler: Arrows: Exports](
    tag: RawTag
  ): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
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
        } yield Some(
          MatchMismatchModel(ld._1, rd._1, shouldMatch) -> parDesugarPrefixOpt(ld._2, rd._2)
        )

      case ForTag(item, iterable) =>
        desugarize(iterable).map { case (v, p) =>
          Some(ForModel(item, v) -> p)
        }

      case PushToStreamTag(operand, exportTo) =>
        desugarize(operand).map { case (v, p) =>
          Some(PushToStreamModel(v, CallModel.callExport(exportTo)) -> p)
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
        Arrows[S].arrows.flatMap(arrows =>
          arrows.get(funcName) match {
            case Some(fn) =>
              logger.trace(s"Call arrow $funcName")
              desugarize(call).flatMap { case (cm, p) =>
                ArrowInliner
                  .callArrow(fn, cm)
                  .map(body =>
                    Some(EmptyModel -> Option(SeqModel.wrapIfNonEmpty(p.toList :+ body: _*)))
                  )
              }
            case None =>
              logger.error(
                s"Cannot find arrow ${funcName}, available: ${arrows.keys.mkString(", ")}"
              )
              none
          }
        )

      case AssignmentTag(value, assignTo) =>
        for {
          cd <- desugarize(value)
          _ <- Exports[S].resolved(assignTo, cd._1)
        } yield Some(SeqModel -> cd._2)

      case ClosureTag(arrow) =>
        Arrows[S].resolved(arrow).map(_ => None)

      case NextTag(item) =>
        pure(NextModel(item))

      case RestrictionTag(name, isStream) =>
        pure(RestrictionModel(name, isStream))

      case SeqTag => pure(SeqModel)
      case ParTag.Detach => pure(DetachModel)
      case _: ParGroupTag => pure(ParModel)
      case XorTag | XorTag.LeftBiased =>
        // TODO should we do smth with XorTag.LeftBiased?
        pure(XorModel)
      case _: NoExecTag => none
      case _ =>
        logger.warn(s"Tag $tag must have been eliminated at this point")
        none
    }
}
