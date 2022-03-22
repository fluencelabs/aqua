package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{ArrayType, OptionType, StreamType}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.Logging

object RawValueInliner extends Logging {

  import Inline.*

  private[inline] def removeLambda[S: Mangler: Exports: Arrows](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, lambdaM) if lambdaM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> ApplyLambdaRaw.fromChain(VarRaw(nameM, btm), lambdaM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    lambdaAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)] =
    Exports[S].exports.flatMap(exports =>
      raw match {
        case VarRaw(name, t) =>
          val vm = VarModel(name, t, Chain.empty).resolveWith(exports)
          State.pure(vm -> Inline.empty)

        case LiteralRaw(value, t) =>
          State.pure(LiteralModel(value, t) -> Inline.empty)

        case alr: ApplyLambdaRaw =>
          val (raw, lambda) = alr.unwind
          lambda
            .foldLeft[State[S, (Chain[LambdaModel], Inline)]](
              State.pure(Chain.empty[LambdaModel] -> Inline.empty)
            ) { case (lcm, l) =>
              lcm.flatMap { case (lc, m) =>
                unfoldLambda(l).map { case (lm, mm) =>
                  (lc :+ lm, m |+| mm)
                }
              }
            }
            .flatMap { case (lambdaModel, map) =>
              unfold(raw, lambdaAllowed).flatMap {
                case (v: VarModel, prefix) =>
                  val vm = v.copy(lambda = lambdaModel).resolveWith(exports)
                  if (lambdaAllowed) State.pure(vm -> (prefix |+| map))
                  else
                    removeLambda(vm).map { case (vmm, mpp) =>
                      vmm -> (prefix |+| mpp |+| map)
                    }
                case (v, prefix) =>
                  // What does it mean actually? I've no ides
                  State.pure((v, prefix |+| map))
              }

            }

        case cr @ CollectionRaw(values, boxType) =>
          val prefix = boxType match {
            case _: StreamType => "stream"
            case _: ArrayType => "array"
            case _: OptionType => "option"
          }
          for {
            streamName <- Mangler[S].findAndForbidName(s"$prefix-sugar")

            stream = VarModel(streamName, StreamType(cr.elementType))
            streamExp = CallModel.Export(stream.name, stream.`type`)

            vals <- values
              .traverse(valueToModel(_))
              .map(_.toList)
              .map(Chain.fromSeq)
              .map(_.flatMap { case (v, t) =>
                Chain.fromOption(t) :+ PushToStreamModel(v, streamExp).leaf
              })

            canonName <-
              if (boxType.isStream) State.pure(streamName)
              else Mangler[S].findAndForbidName(streamName)
            canon = CallModel.Export(canonName, boxType)
          } yield VarModel(canonName, boxType) -> Inline.tree(
            boxType match {
              case ArrayType(_) =>
                RestrictionModel(streamName, isStream = true).wrap(
                  SeqModel.wrap((vals :+ CanonicalizeModel(stream, canon).leaf).toList: _*)
                )
              case OptionType(_) =>
                RestrictionModel(streamName, isStream = true).wrap(
                  SeqModel.wrap(
                    XorModel.wrap((vals :+ NullModel.leaf).toList: _*),
                    CanonicalizeModel(stream, canon).leaf
                  )
                )
              case StreamType(_) =>
                SeqModel.wrap(vals.toList: _*)
            }
          )

        case cr: CallArrowRaw =>
          Mangler[S]
            .findAndForbidName(cr.name)
            .flatMap(n =>
              unfoldArrow(cr, Call.Export(n, cr.`type`) :: Nil).map {
                case (Nil, inline) => (VarModel(n, cr.`type`), inline)
                case (h :: _, inline) => (h, inline)
              }
            )

      }
    )

  private[inline] def unfoldArrow[S: Mangler: Exports: Arrows](
    value: CallArrowRaw,
    exportTo: List[Call.Export]
  ): State[S, (List[ValueModel], Inline)] = {
    val call = Call(value.arguments, exportTo)
    value.serviceId match {
      case Some(serviceId) =>
        for {
          cd <- callToModel(call)
          sd <- valueToModel(serviceId)
        } yield cd._1.exportTo.map(_.asVar) -> Inline(
          Map.empty,
          Chain.fromOption(
            parDesugarPrefixOpt(
              sd._2,
              cd._2
            )
          ) :+ CallServiceModel(sd._1, value.name, cd._1).leaf
        )
      case None =>
        /**
         * Here the back hop happens from [[TagInliner]] to [[ArrowInliner.callArrow]]
         */
        val funcName = value.ability.fold(value.name)(_ + "." + value.name)
        logger.trace(s"            $funcName")
        Arrows[S].arrows.flatMap(arrows =>
          arrows.get(funcName) match {
            case Some(fn) =>
              logger.trace(s"Call arrow $funcName")
              callToModel(call).flatMap { case (cm, p) =>
                ArrowInliner
                  .callArrow(fn, cm)
                  .map(body =>
                    cm.exportTo.map(_.asVar) -> Inline(Map.empty, Chain.fromSeq(p.toList :+ body))
                  )
              }
            case None =>
              logger.error(
                s"Inlining, cannot find arrow ${funcName}, available: ${arrows.keys
                  .mkString(", ")}"
              )
              State.pure(Nil -> Inline.empty)
          }
        )
    }
  }

  private[inline] def unfoldLambda[S: Mangler: Exports: Arrows](
    l: LambdaRaw
  ): State[S, (LambdaModel, Inline)] = // TODO lambda for collection
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case IntoIndexRaw(vm: ApplyLambdaRaw, t) =>
        for {
          nn <- Mangler[S].findAndForbidName("ap-lambda")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: VarRaw, t) =>
        unfold(vr, lambdaAllowed = false).map {
          case (VarModel(name, _, _), map) => IntoIndexModel(name, t) -> map
          case (LiteralModel(v, _), map) => IntoIndexModel(v, t) -> map
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private[inline] def inlineToTree[S: Mangler: Exports: Arrows](
    inline: Inline
  ): State[S, List[OpModel.Tree]] =
    inline.flattenValues.toList.traverse { case (name, v) =>
      valueToModel(v).map {
        case (vv, Some(op)) =>
          SeqModel.wrap(op, FlattenModel(vv, name).leaf)

        case (vv, _) =>
          FlattenModel(vv, name).leaf
      }
    }.map(inline.predo.toList ::: _)

  def valueToModel[S: Mangler: Exports: Arrows](
    value: ValueRaw
  ): State[S, (ValueModel, Option[OpModel.Tree])] =
    for {
      vmp <- unfold(value)
      (vm, map) = vmp

      _ = logger.trace("RAW " + value)
      _ = logger.trace("MOD " + vm)
      dc <- Exports[S].exports
      _ = logger.trace("DEC " + dc)

      ops <- inlineToTree(map)
      _ = logger.trace("desugarized ops: " + ops)
      _ = logger.trace("map was: " + map)
    } yield vm -> parDesugarPrefix(ops)

  def valueListToModel[S: Mangler: Exports: Arrows](
    values: List[ValueRaw]
  ): State[S, List[(ValueModel, Option[OpModel.Tree])]] =
    values.traverse(valueToModel(_))

  def callToModel[S: Mangler: Exports: Arrows](
    call: Call
  ): State[S, (CallModel, Option[OpModel.Tree])] =
    valueListToModel(call.args).map { list =>
      (
        CallModel(
          list.map(_._1),
          call.exportTo.map(CallModel.callExport)
        ),
        parDesugarPrefix(list.flatMap(_._2))
      )
    }
}
