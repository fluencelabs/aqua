package aqua.model.inline

import aqua.model.*
import aqua.model.inline.Inline.MergeMode.*
import aqua.model.inline.raw.*
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{ArrayType, LiteralType, OptionType, StreamType}

import cats.Eval
import cats.data.{Chain, State, StateT}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import scribe.Logging

object RawValueInliner extends Logging {

  import aqua.model.inline.Inline.*

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    propertiesAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)] = for {
    optimized <- StateT.liftF(Optimization.optimize(raw))
    _ <- StateT.liftF(Eval.later(logger.trace("OPTIMIZIED " + optimized)))
    result <- optimized match {
      case VarRaw(name, t) =>
        for {
          exports <- Exports[S].exports
          model = VarModel(name, t, Chain.empty).resolveWith(exports)
        } yield model -> Inline.empty

      case LiteralRaw(value, t) =>
        State.pure(LiteralModel(value, t) -> Inline.empty)

      case alr: ApplyPropertyRaw =>
        ApplyPropertiesRawInliner(alr, propertiesAllowed)

      case cr: CollectionRaw =>
        CollectionRawInliner(cr, propertiesAllowed)

      case sr: StreamRaw =>
        StreamRawInliner(sr, propertiesAllowed)

      case dr: MakeStructRaw =>
        MakeStructRawInliner(dr, propertiesAllowed)

      case ar: AbilityRaw =>
        MakeAbilityRawInliner(ar, propertiesAllowed)

      case auor: ApplyUnaryOpRaw =>
        ApplyUnaryOpRawInliner(auor, propertiesAllowed)

      case abbor: ApplyBinaryOpRaw =>
        ApplyBinaryOpRawInliner(abbor, propertiesAllowed)

      case cr: CallArrowRaw =>
        CallArrowRawInliner(cr, propertiesAllowed)

      case cs: CallServiceRaw =>
        CallServiceRawInliner(cs, propertiesAllowed)

    }
  } yield result

  private[inline] def inlineToTree[S: Mangler: Exports: Arrows](
    inline: Inline
  ): State[S, List[OpModel.Tree]] =
    (inline.mergeMode match {
      case SeqMode => SeqModel.wrap(inline.predo) :: Nil
      case ParMode => inline.predo.toList
    }).pure

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
    } yield vm -> parDesugarPrefix(ops.filterNot(_ == EmptyModel.leaf))

  def valueToModel[S: Mangler: Exports: Arrows](
    value: ValueRaw,
    propertiesAllowed: Boolean = true
  ): State[S, (ValueModel, Option[OpModel.Tree])] = for {
    _ <- StateT.liftF(Eval.later(logger.trace("RAW " + value)))
    model <- toModel(unfold(value, propertiesAllowed))
  } yield model

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
  ): State[S, (CallModel, Option[OpModel.Tree])] = {
    for {
      args <- valueListToModel(call.args)
      args <- {
        if (flatStreamArguments)
          args.traverse(TagInliner.flat.tupled)
        else
          State.pure(args)
      }
      exportTo <- call.exportTo.traverse {
        case c@Call.Export(_, _, isExistingStream) if isExistingStream =>
          // process streams, because they can be stored in Exports outside function/closure with different name
          valueToModel(c.toRaw)
        case ce =>
          State.pure((VarModel(ce.name, ce.`type`), None))
      }
    } yield {
      val (argsVars, argsOps) = args.unzip.map(_.flatten)
      val (exportVars, exportOps) = exportTo.unzip.map(_.flatten)
      val exportModel = exportVars.collect {
        // exportTo can be only a variable
        case VarModel(name, baseType, _) => CallModel.Export(name, baseType)
      }
      (
        CallModel(
          argsVars,
          exportModel
        ),
        parDesugarPrefix(exportOps ++ argsOps)
      )
    }
  }
}
