package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.model.inline.raw.{ApplyFunctorRawInliner, ApplyGateRawInliner, ApplyPropertiesRawInliner, CallArrowRawInliner, CollectionRawInliner}
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{ArrayType, OptionType, StreamType}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.Logging

object RawValueInliner extends Logging {

  import Inline.*

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    propertiesAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)] =
    raw match {
      case VarRaw(name, t) =>
        Exports[S].exports.map(VarModel(name, t, Chain.empty).resolveWith).map(_ -> Inline.empty)

      case LiteralRaw(value, t) =>
        State.pure(LiteralModel(value, t) -> Inline.empty)

      case alr: ApplyPropertyRaw =>
        ApplyPropertiesRawInliner(alr, propertiesAllowed)

      case agr: ApplyGateRaw =>
        ApplyGateRawInliner(agr, propertiesAllowed)

      case cr: CollectionRaw =>
        CollectionRawInliner(cr, propertiesAllowed)

      case dr: MakeStructRaw =>
        MakeStructRawInliner(dr, propertiesAllowed)

      case cr: CallArrowRaw =>
        CallArrowRawInliner(cr, propertiesAllowed)

      case sr: ShadowRaw =>
        // First, collect shadowed values
        // TODO: might be already defined in scope!
        sr.shadowValues.toList
          // Unfold/substitute all shadowed value
          .traverse { case (name, v) =>
            unfold(v, propertiesAllowed).map { case (svm, si) =>
              (name, svm, si)
            }
          }.flatMap { fas =>
            val res = fas.map { case (n, v, _) =>
              n -> v
            }.toMap
            // Mark shadowed values as exports, isolate them into a scope
            Exports[S].exports
              .flatMap(curr =>
                Exports[S]
                  .scope(
                    Exports[S].resolved(res ++ curr.view.mapValues(_.resolveWith(res))) >>
                      // Resolve the value in the prepared Exports scope
                      unfold(sr.value, propertiesAllowed)
                  )
              )
              .map { case (vm, inl) =>
                // Collect inlines to prepend before the value
                (vm, fas.map(_._3).foldLeft(inl)(_ |+| _))
              }
          }
    }

  private[inline] def inlineToTree[S: Mangler: Exports: Arrows](
    inline: Inline
  ): State[S, List[OpModel.Tree]] = {
    inline.flattenValues.toList.traverse { case (name, v) =>
      valueToModel(v).map {
        case (vv, Some(op)) =>
          SeqModel.wrap(op, FlattenModel(vv, name).leaf)

        case (vv, _) =>
          FlattenModel(vv, name).leaf
      }
    }.map{ predo =>
      inline.mergeMode match
        case SeqMode =>
          SeqModel.wrap((inline.predo.toList ++ predo):_*) :: Nil
        case ParMode => inline.predo.toList ::: predo
    }
  }

  private[inline] def toModel[S: Mangler: Exports: Arrows](
    unfoldF: State[S, (ValueModel, Inline)]
  ): State[S, (ValueModel, Option[OpModel.Tree])] =
    for {
      vmp <- unfoldF
      (vm, map) = vmp
      _ = logger.trace("MOD " + vm)
      dc <- Exports[S].exports
      _ = logger.trace("DEC " + dc)

      ops <- inlineToTree(map)
      _ = logger.trace("desugarized ops: " + ops)
      _ = logger.trace("map was: " + map)
    } yield vm -> parDesugarPrefix(ops)

  def collectionToModel[S: Mangler: Exports: Arrows](
    value: CollectionRaw,
    assignTo: Option[String]
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    logger.trace("RAW COLLECTION " + value)
    toModel(CollectionRawInliner.unfoldCollection(value, assignTo))
  }

  def valueToModel[S: Mangler: Exports: Arrows](
    value: ValueRaw,
    propertiesAllowed: Boolean = true
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    logger.trace("RAW " + value)
    toModel(unfold(value, propertiesAllowed))
  }

  def valueListToModel[S: Mangler: Exports: Arrows](
    values: List[ValueRaw]
  ): State[S, List[(ValueModel, Option[OpModel.Tree])]] =
    values.traverse(valueToModel(_))

  /**
   * Unfold all arguments and make CallModel
   * @param flatStreamArguments canonicalize and flatten all stream arguments if true
   */
  def callToModel[S: Mangler: Exports: Arrows](
    call: Call,
    flatStreamArguments: Boolean
  ): State[S, (CallModel, Option[OpModel.Tree])] =
    valueListToModel(call.args).flatMap { args =>
      if (flatStreamArguments)
        args.map(arg => TagInliner.flat(arg._1, arg._2, true)).sequence
      else
        State.pure(args)
    }.map { list =>
      (
        CallModel(
          list.map(_._1),
          call.exportTo.map(CallModel.callExport)
        ),
        parDesugarPrefix(list.flatMap(_._2))
      )
    }
}
