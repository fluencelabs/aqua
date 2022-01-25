package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.*
import aqua.raw.value.*
import cats.syntax.traverse.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.Logging

object RawValueInliner extends Logging {

  import Inline.*

  private[inline] def unfold[S: Mangler: Exports](
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
              vm -> map
            }
      }
    )

  private[inline] def unfoldLambda[S: Mangler](
    l: LambdaRaw
  ): State[S, (LambdaModel, Map[String, ValueRaw])] =
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Map.empty)
      case IntoIndexRaw(vm @ VarRaw(name, _, l), t) if l.nonEmpty =>
        for {
          nn <- Mangler[S].findNewName(name)
          _ <- Mangler[S].forbid(Set(nn))
        } yield IntoIndexModel(nn, t) -> Map(nn -> vm)

      case IntoIndexRaw(VarRaw(name, _, _), t) =>
        State.pure(IntoIndexModel(name, t) -> Map.empty)

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Map.empty)
    }

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

      ops <- map.toList.traverse { case (name, v) =>
        valueToModel(v).map {
          case (vv, Some(op)) =>
            SeqModel.wrap(op, FlattenModel(vv, name).leaf)

          case (vv, _) =>
            FlattenModel(vv, name).leaf
        }
      }
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
