package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{ArrayType, StreamType}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.Logging

object RawValueInliner extends Logging {

  import Inline.*

  private[inline] def removeLambda[S: Mangler: Exports](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, lambdaM) if lambdaM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> VarRaw(nameM, btm, lambdaM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private[inline] def unfold[S: Mangler: Exports](
    raw: ValueRaw,
    lambdaAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)] =
    Exports[S].exports.flatMap(exports =>
      raw match {
        case VarRaw(name, t, lambda) if lambda.isEmpty =>
          val vm = VarModel(name, t, Chain.empty).resolveWith(exports)
          if (lambdaAllowed) State.pure(vm -> Inline.empty) else removeLambda(vm)

        case LiteralRaw(value, t) =>
          State.pure(LiteralModel(value, t) -> Inline.empty)

        case VarRaw(name, t, lambda) =>
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
              val vm = VarModel(name, t, lambdaModel).resolveWith(exports)
              if (lambdaAllowed) State.pure(vm -> map)
              else
                removeLambda(vm).map { case (vmm, mpp) =>
                  vmm -> (mpp |+| map)
                }
            }

        case cr @ CollectionRaw(values) =>
          for {
            streamName <- Mangler[S].findAndForbidName("stream-sugar")

            stream = VarModel(streamName, StreamType(cr.elementType))
            streamExp = CallModel.Export(stream.name, stream.`type`)

            vals <- values
              .traverse(valueToModel(_))
              .map(_.toList)
              .map(Chain.fromSeq)
              .map(_.flatMap { case (v, t) =>
                Chain.fromOption(t) :+ PushToStreamModel(v, streamExp).leaf
              })

            canonName <- Mangler[S].findAndForbidName("stream-sugar-can")
            canon = CallModel.Export(canonName, cr.`type`)
          } yield VarModel(canonName, cr.`type`) -> Inline.tree(
            RestrictionModel(streamName, isStream = true).wrap(
              SeqModel.wrap((vals :+ CanonicalizeModel(stream, canon).leaf).toList: _*)
            )
          )

      }
    )

  private[inline] def unfoldLambda[S: Mangler: Exports](
    l: LambdaRaw
  ): State[S, (LambdaModel, Inline)] = // TODO lambda for collection
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case IntoIndexRaw(vm @ VarRaw(name, _, l), t) if l.nonEmpty =>
        for {
          nn <- Mangler[S].findNewName(name)
          _ <- Mangler[S].forbid(Set(nn))
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: VarRaw, t) =>
        unfold(vr, lambdaAllowed = false).map {
          case (VarModel(name, _, _), map) => IntoIndexModel(name, t) -> map
          case (LiteralModel(v, _), map) => IntoIndexModel(v, t) -> map
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private[inline] def inlineToTree[S: Mangler: Exports](
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

  def valueToModel[S: Mangler: Exports](
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

  def valueListToModel[S: Mangler: Exports](
    values: List[ValueRaw]
  ): State[S, List[(ValueModel, Option[OpModel.Tree])]] =
    values.traverse(valueToModel(_))

  def callToModel[S: Mangler: Exports](
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
