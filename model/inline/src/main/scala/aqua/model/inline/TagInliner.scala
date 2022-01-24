package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.*
import aqua.raw.value.*
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

  import Inline.*

  private def pure[S](op: OpModel): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(Some(op -> None))

  private def none[S]: State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    State.pure(None)

  /**
   * Processes a single [[RawTag]] that may lead to many changes, including calling [[ArrowInliner]]
   *
   * @param tag Tag to process
   * @tparam S Current state
   * @return Model (if any), and prefix (if any)
   */
  def tagToModel[S: Counter : Mangler : Arrows : Exports](
                                                           tag: RawTag
                                                         ): State[S, Option[(OpModel, Option[OpModel.Tree])]] =
    tag match {
      case OnTag(peerId, via) =>
        for {
          peerIdDe <- valueToModel(peerId)
          viaDe <- valueListToModel(via.toList)
          (pid, pif) = peerIdDe
          viaD = Chain.fromSeq(viaDe.map(_._1))
          viaF = viaDe.flatMap(_._2)

        } yield Some(OnModel(pid, viaD) -> parDesugarPrefix(viaF.prependedAll(pif)))

      case MatchMismatchTag(left, right, shouldMatch) =>
        for {
          ld <- valueToModel(left)
          rd <- valueToModel(right)
        } yield Some(
          MatchMismatchModel(ld._1, rd._1, shouldMatch) -> parDesugarPrefixOpt(ld._2, rd._2)
        )

      case ForTag(item, iterable) =>
        valueToModel(iterable).map { case (v, p) =>
          Some(ForModel(item, v) -> p)
        }

      case PushToStreamTag(operand, exportTo) =>
        valueToModel(operand).map { case (v, p) =>
          Some(PushToStreamModel(v, CallModel.callExport(exportTo)) -> p)
        }

      case CallServiceTag(serviceId, funcName, call) =>
        for {
          cd <- callToModel(call)
          sd <- valueToModel(serviceId)
        } yield Some(CallServiceModel(sd._1, funcName, cd._1) -> parDesugarPrefixOpt(sd._2, cd._2))

      case CanonicalizeTag(operand, exportTo) =>
        valueToModel(operand).map { case (v, p) =>
          Some(CanonicalizeModel(v, CallModel.callExport(exportTo)) -> p)
        }

      case JoinTag(operands) =>
        operands
          .traverse(valueToModel)
          .map(nel => Some(JoinModel(nel.map(_._1)) -> parDesugarPrefix(nel.toList.flatMap(_._2))))

      case CallArrowTag(funcName, call) =>

        /**
         * Here the back hop happens from [[TagInliner]] to [[ArrowInliner.callArrow]]
         */
        logger.trace(s"            $funcName")
        Arrows[S].arrows.flatMap(arrows =>
          arrows.get(funcName) match {
            case Some(fn) =>
              logger.trace(s"Call arrow $funcName")
              callToModel(call).flatMap { case (cm, p) =>
                ArrowInliner
                  .callArrow(fn, cm)
                  .map(body => Some(EmptyModel -> Option(SeqModel.wrap(p.toList :+ body: _*))))
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
          cd <- valueToModel(value)
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

  private def traverseS[S](
                            cf: RawTag.Tree,
                            f: RawTag => State[S, OpModel.Tree]
                          ): State[S, OpModel.Tree] =
    for {
      headTree <- f(cf.head)
      tail <- StateT.liftF(cf.tail)
      tailTree <- tail.traverse(traverseS[S](_, f))
    } yield headTree.copy(tail = headTree.tail.map(_ ++ tailTree))

  private def handleTag[S: Exports : Counter : Arrows : Mangler](tag: RawTag): State[S, OpModel.Tree] =
    for {
      resolvedArrows <- Arrows[S].arrows

      opModelAndPrefixTree <- TagInliner.tagToModel(tag)
      dPrefix = opModelAndPrefixTree.flatMap(_._2)
      dTag = opModelAndPrefixTree.map(_._1)

    } yield
      // If smth needs to be added before this function tree, add it with Seq
      SeqModel.wrap(dPrefix.toList ::: dTag.map(_.leaf).toList: _*)

  def handleTree[S: Exports : Counter : Mangler : Arrows](
                                                           tree: RawTag.Tree
                                                         ): State[S, OpModel.Tree] =
    traverseS(tree, handleTag(_))
}
