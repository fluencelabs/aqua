package aqua.model.inline

import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.*
import aqua.model.inline.RawValueInliner.collectionToModel
import aqua.model.inline.raw.CallArrowRawInliner
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{BoxType, CanonStreamType, StreamType}
import cats.syntax.traverse.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.Logging

/**
 * [[TagInliner]] prepares a [[RawTag]] for futher processing by converting [[ValueRaw]]s into [[ValueModel]]s.
 *
 * Converts [[ValueRaw]]s into [[ValueModel]]s with [[RawValueInliner]]
 * Inlines [[CallArrowTag]] using [[ArrowInliner]]
 *
 * Doing so might require some tree, expressed as [[OpModel.Tree]], to be prepended before the
 * resulting node. Hence the return types: (model, Option(tree to prepend)).
 */
object TagInliner extends Logging {

  import RawValueInliner.{callToModel, valueListToModel, valueToModel}

  import aqua.model.inline.Inline.*

  private def pure[S](op: OpModel): State[S, (Option[OpModel], Option[OpModel.Tree])] =
    State.pure(Some(op) -> None)

  private def none[S]: State[S, (Option[OpModel], Option[OpModel.Tree])] =
    State.pure(None -> None)

  private def combineOpsWithSeq(l: Option[OpModel.Tree], r: Option[OpModel.Tree]) =
    l match {
      case None => r
      case Some(a) =>
        r match {
          case None => l
          case Some(b) => Some(SeqModel.wrap(a, b))
        }
    }

  def canonicalizeIfStream[S: Mangler](
    vm: ValueModel,
    ops: Option[OpModel.Tree]
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    vm match {
      case VarModel(name, StreamType(el), l) =>
        val canonName = name + "_canon"
        Mangler[S].findAndForbidName(canonName).map { n =>
          val canon = VarModel(n, CanonStreamType(el), l)
          val canonModel = CanonicalizeModel(vm, CallModel.Export(canon.name, canon.`type`)).leaf
          canon -> combineOpsWithSeq(ops, Option(canonModel))
        }
      case _ => State.pure(vm -> ops)
    }
  }

  def flat[S: Mangler](
    vm: ValueModel,
    op: Option[OpModel.Tree],
    flatStream: Boolean
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    vm match {
      case v @ VarModel(n, StreamType(t), l) if flatStream =>
        val canonName = n + "_canon"
        for {
          canonN <- Mangler[S].findAndForbidName(canonName)
          canonV = VarModel(canonN, CanonStreamType(t), l)
          canonOp = CanonicalizeModel(
            v.copy(properties = Chain.empty),
            CallModel.Export(canonV.name, canonV.baseType)
          ).leaf
          flatResult <- flatCanonStream(canonV, Some(canonOp))
        } yield {
          val (resV, resOp) = flatResult
          (resV, combineOpsWithSeq(op, resOp))
        }
      case v @ VarModel(_, CanonStreamType(_), _) =>
        flatCanonStream(v, op)
      case _ => State.pure((vm, op))
    }
  }

  private def flatCanonStream[S: Mangler](
    canonV: VarModel,
    op: Option[OpModel.Tree]
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    if (canonV.properties.nonEmpty) {
      val apName = canonV.name + "_flatten"
      Mangler[S].findAndForbidName(apName).map { apN =>
        val apV = VarModel(apN, canonV.`type`)
        val apOp = FlattenModel(canonV, apN).leaf
        (
          apV,
          combineOpsWithSeq(op, Option(apOp))
        )
      }
    } else {
      State.pure((canonV, op))
    }

  }

  /**
   * Processes a single [[RawTag]] that may lead to many changes, including calling [[ArrowInliner]]
   *
   * @param tag Tag to process
   * @tparam S Current state
   * @return Model (if any), and prefix (if any)
   */
  def tagToModel[S: Mangler: Arrows: Exports](
    tag: RawTag,
    treeFunctionName: String
  ): State[S, (Option[OpModel], Option[OpModel.Tree])] =
    tag match {
      case OnTag(peerId, via) =>
        for {
          peerIdDe <- valueToModel(peerId)
          viaDe <- valueListToModel(via.toList)
          viaDeFlattened <- viaDe.traverse { case (vm, tree) =>
            flat(vm, tree, true)
          }
          (pid, pif) = peerIdDe
          viaD = Chain.fromSeq(viaDeFlattened.map(_._1))
          viaF = viaDeFlattened.flatMap(_._2)

        } yield Some(OnModel(pid, viaD)) -> parDesugarPrefix(viaF.prependedAll(pif))

      case MatchMismatchTag(left, right, shouldMatch) =>
        for {
          ld <- valueToModel(left)
          rd <- valueToModel(right)
          ldCanon <- canonicalizeIfStream(ld._1, ld._2)
          rdCanon <- canonicalizeIfStream(rd._1, rd._2)
        } yield Some(
          MatchMismatchModel(ldCanon._1, rdCanon._1, shouldMatch)
        ) -> parDesugarPrefixOpt(
          ldCanon._2,
          rdCanon._2
        )

      case ForTag(item, iterable, mode) =>
        for {
          vp <- valueToModel(iterable)
          flattened <- flat(vp._1, vp._2, true)
          (v, p) = flattened
          n <- Mangler[S].findAndForbidName(item)
          elementType = iterable.`type` match {
            case b: BoxType => b.element
            // TODO: it is unexpected, should we handle this?
            case _ =>
              logger.error(
                s"Unexpected behaviour: non-box type variable '$iterable' in 'for' expression."
              )
              iterable.`type`
          }
          _ <- Exports[S].resolved(item, VarModel(n, elementType))
        } yield {
          val m = mode.map {
            case ForTag.WaitMode => ForModel.NeverMode
            case ForTag.PassMode => ForModel.NullMode
          }

          Some(ForModel(n, v, m)) -> p
        }

      case PushToStreamTag(operand, exportTo) =>
        valueToModel(operand).map { case (v, p) =>
          Some(PushToStreamModel(v, CallModel.callExport(exportTo))) -> p
        }

      case CanonicalizeTag(operand, exportTo) =>
        valueToModel(operand).flatMap {
          // pass literals as is
          case (l @ LiteralModel(_, _), p) =>
            for {
              _ <- Exports[S].resolved(exportTo.name, l)
            } yield None -> p
          case (v, p) =>
            State.pure(Some(CanonicalizeModel(v, CallModel.callExport(exportTo))) -> p)
        }

      case FlattenTag(operand, assignTo) =>
        valueToModel(operand).flatMap {
          // pass literals as is
          case (l @ LiteralModel(_, _), p) =>
            for {
              _ <- Exports[S].resolved(assignTo, l)
            } yield None -> p
          case (v, p) =>
            State.pure(Some(FlattenModel(v, assignTo)) -> p)
        }

      case JoinTag(operands) =>
        operands
          .traverse(o => valueToModel(o))
          .map(nel => {
            logger.trace("join after " + nel.map(_._1))
            // None because join behaviour will be processed in ApplyPropertiesRawInliner
            None -> parDesugarPrefix(nel.toList.flatMap(_._2))
          })

      case CallArrowRawTag(exportTo, value: CallArrowRaw) =>
        CallArrowRawInliner.unfoldArrow(value, exportTo).flatMap { case (_, inline) =>
          RawValueInliner.inlineToTree(inline).map(tree => (None, Some(SeqModel.wrap(tree: _*))))
        }

      case AssignmentTag(value, assignTo) =>
        (value match {
          // if we assign collection to a stream, we must use it's name, because it is already created with 'new'
          case c @ CollectionRaw(_, _: StreamType) =>
            collectionToModel(c, Some(assignTo))
          case v =>
            valueToModel(v, false)
        }).flatMap { case (model, prefix) =>
          for {
            // NOTE: Name <assignTo> should not exist yet
            _ <- Mangler[S].forbidName(assignTo)
            _ <- Exports[S].resolved(assignTo, model)
          } yield None -> prefix
        }

      case ClosureTag(arrow, detach) =>
        if (detach) Arrows[S].resolved(arrow, None).map(_ => None -> None)
        else
          for {
            t <- Mangler[S].findAndForbidName(arrow.name)
            _ <- Arrows[S].resolved(arrow, Some(t))
          } yield Some(CaptureTopologyModel(t)) -> None

      case NextTag(item) =>
        for {
          exps <- Exports[S].exports
        } yield {
          exps.get(item).collect { case VarModel(n, _, _) =>
            NextModel(n)
          } -> None
        }

      case RestrictionTag(name, isStream) =>
        pure(RestrictionModel(name, isStream))

      case _: SeqGroupTag => pure(SeqModel)
      case ParTag.Detach => pure(DetachModel)
      case _: ParGroupTag => pure(ParModel)
      case XorTag | XorTag.LeftBiased =>
        pure(XorModel)
      case DeclareStreamTag(value) =>
        value match
          case VarRaw(name, _) =>
            for {
              cd <- valueToModel(value)
              _ <- Exports[S].resolved(name, cd._1)
            } yield None -> cd._2
          case _ => none

      case _: NoExecTag => none
      case _ =>
        logger.warn(s"Tag $tag must have been eliminated at this point")
        none
    }

  private def traverseS[S](
    cf: RawTag.Tree,
    f: RawTag => State[S, (Option[OpModel], Option[OpModel.Tree])]
  ): State[S, OpModel.Tree] =
    for {
      headTree <- f(cf.head)
      tail <- StateT.liftF(cf.tail)
      tailTree <- tail.traverse(traverseS[S](_, f))
    } yield headTree match {
      case (Some(m), prefix) => SeqModel.wrap(prefix.toList :+ m.wrap(tailTree.toList: _*): _*)
      case (None, prefix) => SeqModel.wrap(prefix.toList ++ tailTree.toList: _*)
    }

  def handleTree[S: Exports: Mangler: Arrows](
    tree: RawTag.Tree,
    treeFunctionName: String
  ): State[S, OpModel.Tree] =
    traverseS(tree, tagToModel(_, treeFunctionName))
}
