package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.model.inline.raw.{ApplyLambdaRawInliner, CallArrowRawInliner, CollectionRawInliner}
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

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    lambdaAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)] =
    raw match {
      case VarRaw(name, t) =>
        Exports[S].exports.map(VarModel(name, t, Chain.empty).resolveWith).map(_ -> Inline.empty)

      case LiteralRaw(value, t) =>
        State.pure(LiteralModel(value, t) -> Inline.empty)

      case alr: ApplyLambdaRaw =>
        ApplyLambdaRawInliner(alr, lambdaAllowed)

      case cr: CollectionRaw =>
        CollectionRawInliner(cr, lambdaAllowed)

      case cr: CallArrowRaw =>
        CallArrowRawInliner(cr, lambdaAllowed)
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

      _ = println("RAW " + value)
      _ = println("MOD " + vm)
      dc <- Exports[S].exports
      _ = println("DEC " + dc)

      ops <- inlineToTree(map)
      _ = println("desugarized ops: " + ops)
      _ = println("map was: " + map)
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
