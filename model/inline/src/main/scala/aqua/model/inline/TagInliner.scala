package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.model.inline.raw.CallArrowRawInliner
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{ArrayType, BoxType, CanonStreamType, StreamType}
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import scribe.{Logging, log}

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

  import Inline.*

  private def pure[S](op: OpModel): State[S, (Option[OpModel], Option[OpModel.Tree])] =
    State.pure(Some(op) -> None)

  private def none[S]: State[S, (Option[OpModel], Option[OpModel.Tree])] =
    State.pure(None -> None)

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
            vm match {
              // flatten CanonStream, because in via we are using `fold`
              // and `fold` cannot use CanonStream with lambda
              case VarModel(n, CanonStreamType(_), l) if l.nonEmpty =>
                val apName = n + "_flatten"
                Mangler[S].findAndForbidName(apName).map {s =>
                  val apV = VarModel(s, vm.`type`)
                  val apOp = FlattenModel(vm, s).leaf
                  val op = Option(tree.fold(apOp)(t => SeqModel.wrap(t, apOp)))
                  (apV, op)
                }

              case _ => State.pure((vm, tree))
            }
          }
          (pid, pif) = peerIdDe
          viaD = Chain.fromSeq(viaDeFlattened.map(_._1))
          viaF = viaDeFlattened.flatMap(_._2)

        } yield Some(OnModel(pid, viaD)) -> parDesugarPrefix(viaF.prependedAll(pif))

      case MatchMismatchTag(left, right, shouldMatch) =>
        for {
          ld <- valueToModel(left)
          rd <- valueToModel(right)
        } yield Some(MatchMismatchModel(ld._1, rd._1, shouldMatch)) -> parDesugarPrefixOpt(
          ld._2,
          rd._2
        )

      case ForTag(item, iterable) =>
        for {
          vp <- valueToModel(iterable)
          (v, p) = vp
          n <- Mangler[S].findAndForbidName(item)
          _ = println("iterable model: " + vp)
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
          Some(ForModel(n, v)) -> p
        }

      case PushToStreamTag(operand, exportTo) =>
        valueToModel(operand).map { case (v, p) =>
          Some(PushToStreamModel(v, CallModel.callExport(exportTo))) -> p
        }

      case CanonicalizeTag(operand, exportTo) =>
        valueToModel(operand).map { case (v, p) =>
          Some(CanonicalizeModel(v, CallModel.callExport(exportTo))) -> p
        }

      case JoinTag(operands) =>
        logger.trace("join " + operands)
        operands
          .traverse(o => valueToModel(o, false))
          .map(nel => {
            logger.trace("join after " + nel.map(_._1))
            Some(JoinModel(nel.map(_._1))) -> parDesugarPrefix(nel.toList.flatMap(_._2))
          })

      case CallArrowRawTag(exportTo, value: CallArrowRaw) =>
        CallArrowRawInliner.unfoldArrow(value, exportTo).flatMap { case (_, inline) =>
          RawValueInliner.inlineToTree(inline).map(tree => (None, Some(SeqModel.wrap(tree: _*))))
        }

      case AssignmentTag(value, assignTo) =>
        for {
          cd <- valueToModel(value)
          _ <- Exports[S].resolved(assignTo, cd._1)
        } yield Some(SeqModel) -> cd._2

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
