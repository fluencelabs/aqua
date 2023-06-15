package aqua.model.transform.topology

import aqua.model.transform.cursor.ChainZipper
import aqua.model.transform.topology.strategy.*
import aqua.model.*
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.res.{ApRes, CanonRes, FoldRes, MakeRes, NextRes, ResolvedOp, SeqRes}
import aqua.types.{ArrayType, BoxType, CanonStreamType, ScalarType, StreamType}
import cats.Eval
import cats.data.Chain.{==:, nil}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.syntax.apply.*
import cats.syntax.option.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*
import cats.instances.map.*
import scribe.Logging

/**
 * Wraps all the logic for topology reasoning about the tag in the AST represented by the [[cursor]]
 *
 * @param cursor
 * Pointer to the current place in the AST
 * @param before
 * Strategy of calculating where the previous executions happened
 * @param begins
 * Strategy of calculating where execution of this tag/node should begin
 * @param ends
 * Strategy of calculating where execution of this tag/node happens
 * @param after
 * Strategy of calculating where the next execution should happen and whether we need to move
 * there or not
 */
case class Topology private (
  cursor: OpModelTreeCursor,
  before: Before,
  begins: Begins,
  ends: Ends,
  after: After
) extends Logging {

  val parent: Option[Topology] = cursor.moveUp.map(_.topology)

  val parents: LazyList[Topology] =
    LazyList.unfold(parent)(p => p.map(pp => pp -> pp.parent))

  // Map of all previously-seen Captured Topologies -- see CaptureTopologyModel, ApplyTopologyModel
  val capturedTopologies: Eval[Map[String, Topology]] =
    cursor.moveLeft
      .traverse(_.topology.capturedTopologies)
      .map(_.getOrElse(Map.empty))
      .flatMap(tops =>
        cursor.op match {
          case CaptureTopologyModel(name) =>
            logger.trace(s"Capturing topology `$name`")
            Eval.now(tops + (name -> this))
          case x =>
            logger.trace(s"Skip $x")
            cursor.toLastChild
              .traverse(_.topology.capturedTopologies)
              .map(_.getOrElse(Map.empty) ++ tops)
        }
      )
      .memoize

  // Current topology location – stack of OnModel's collected from parents branch
  // ApplyTopologyModel shifts topology to pathOn where this topology was Captured
  val pathOn: Eval[List[OnModel]] = Eval
    .defer(
      cursor.op match {
        case o: OnModel =>
          parent.traverse(_.pathOn).map(pPath => o :: pPath.orEmpty)
        case ApplyTopologyModel(name) =>
          capturedTopologies.flatMap(
            _.get(name)
              .traverse(_.pathOn)
              .flatTap(p =>
                Eval.later(
                  if (p.isEmpty) logger.error(s"Captured topology `$name` not found")
                )
              )
              .map(_.orEmpty)
          )
        case _ => parent.traverse(_.pathOn).map(_.orEmpty)
      }
    )
    .memoize

  // Find path of first `ForceExecModel` (call, canon, join) in this subtree
  lazy val firstExecutesOn: Eval[Option[List[OnModel]]] =
    (cursor.op match {
      case _: ForceExecModel => pathOn.map(_.some)
      case _ => children.collectFirstSomeM(_.firstExecutesOn)
    }).memoize

  // Find path of last `ForceExecModel` (call, canon, join) in this subtree
  lazy val lastExecutesOn: Eval[Option[List[OnModel]]] =
    (cursor.op match {
      case _: ForceExecModel => pathOn.map(_.some)
      case _ => children.reverse.collectFirstSomeM(_.lastExecutesOn)
    }).memoize

  lazy val currentPeerId: Option[ValueModel] = pathOn.value.headOption.map(_.peerId)

  // noExec nodes are meaningless topology-wise, so filter them out
  lazy val prevSibling: Option[Topology] = cursor.toPrevSibling.flatMap {
    case c if c.isNoExec => c.topology.prevSibling
    case c => Some(c.topology)
  }

  lazy val nextSibling: Option[Topology] = cursor.toNextSibling.flatMap {
    case c if c.isNoExec => c.topology.nextSibling
    case c => Some(c.topology)
  }

  lazy val firstChild: Option[Topology] = cursor.toFirstChild.flatMap {
    case c if c.isNoExec => c.topology.nextSibling
    case c => Some(c.topology)
  }

  lazy val lastChild: Option[Topology] = cursor.toLastChild.flatMap {
    case c if c.isNoExec => c.topology.prevSibling
    case c => Some(c.topology)
  }

  lazy val children: LazyList[Topology] = cursor.children.filterNot(_.isNoExec).map(_.topology)

  def findInside(f: Topology => Boolean): LazyList[Topology] =
    children.flatMap(_.findInside(f)).prependedAll(Option.when(f(this))(this))

  lazy val forModel: Option[ForModel] = Option(cursor.op).collect { case ft: ForModel =>
    ft
  }

  lazy val isForModel: Boolean = forModel.isDefined

  // Before the left boundary of this element, what was the scope
  lazy val beforeOn: Eval[List[OnModel]] = before.beforeOn(this).memoize

  // Inside the left boundary of this element, what should be the scope
  lazy val beginsOn: Eval[List[OnModel]] = begins.beginsOn(this).memoize

  // After this element is done, what is the scope
  lazy val endsOn: Eval[List[OnModel]] = ends.endsOn(this).memoize

  // After this element is done, where should it move to prepare for the next one
  lazy val afterOn: Eval[List[OnModel]] = after.afterOn(this).memoize

  // Usually we don't care about exiting from where this tag ends into the outer scope
  // But for some cases, like par branches, its necessary, so the exit can be forced
  lazy val forceExit: Eval[Boolean] = after.forceExit(this).memoize

  // Where we finally are, after exit enforcement is applied
  lazy val finallyOn: Eval[List[OnModel]] = after.finallyOn(this).memoize

  lazy val pathBefore: Eval[Chain[ValueModel]] = begins.pathBefore(this).memoize

  lazy val pathAfter: Eval[Chain[ValueModel]] = after.pathAfter(this).memoize

  override def toString: String =
    s"Topology(${cursor},\n\tbefore: ${before},\n\tbegins: $begins,\n\t  ends: $ends,\n\t after:$after)"
}

object Topology extends Logging {
  type Res = ResolvedOp.Tree

  def findRelayPathEnforcement(bef: List[OnModel], beg: List[OnModel]): Chain[ValueModel] =
    Chain.fromOption(
      beg.headOption
        .map(_.peerId)
        .filter(lastPeerId => beg.tail.headOption.exists(_.via.lastOption.contains(lastPeerId)))
        .filter(lastPeerId => !bef.headOption.exists(_.peerId == lastPeerId))
    )

  def make(cursor: OpModelTreeCursor): Topology =
    Topology(
      cursor,
      // Before
      cursor.parentOp match {
        case Some(XorModel) => XorBranch
        case Some(_: SeqGroupModel) => SeqGroupBranch
        case None => Root
        case _ => Default
      },
      // Begin
      (cursor.parentOp, cursor.op) match {
        case (Some(_: SeqGroupModel), _: NextModel) =>
          SeqNext
        case (_, _: ForModel) =>
          For
        case (_, ParModel) => // No begin optimization for detach
          ParGroup
        case _ =>
          Default
      },
      // End
      cursor.op match {
        case _: SeqGroupModel => SeqGroup
        case XorModel => XorGroup
        case _: ParGroupModel => ParGroup
        case _ if cursor.parentOp.isEmpty => Root
        case _ => Default
      },
      // After
      cursor.parentOp match {
        case Some(_: ParGroupModel) => ParGroupBranch
        case Some(XorModel) => XorBranch
        case Some(_: SeqGroupModel) => SeqGroupBranch
        case None => Root
        case _ => Default
      }
    )

  def resolve(op: OpModel.Tree, debug: Boolean = false): Eval[Res] =
    resolveOnMoves(op, debug).flatMap(resolved =>
      Cofree
        .cata[Chain, ResolvedOp, Res](resolved) {
          case (SeqRes, children) =>
            Eval.later {
              children.uncons
                .filter(_._2.isEmpty)
                .map(_._1)
                .getOrElse(
                  Cofree(
                    SeqRes,
                    Eval.now(children.flatMap {
                      case Cofree(SeqRes, ch) => ch.value
                      case cf => Chain.one(cf)
                    })
                  )
                )
            }
          case (head, children) => Eval.later(Cofree(head, Eval.now(children)))
        }
    )

  def wrap(cz: ChainZipper[Res]): Chain[Res] =
    Chain.one(
      if (cz.prev.nonEmpty || cz.next.nonEmpty) Cofree(SeqRes, Eval.now(cz.chain))
      else cz.current
    )

  def resolveOnMoves(op: OpModel.Tree, debug: Boolean): Eval[Res] = {
    val cursor = OpModelTreeCursor(NonEmptyList.one(ChainZipper.one(op)), None)
    // TODO: remove var
    var i = 0

    def nextI = {
      i = i + 1
      i
    }

    val resolvedCofree = cursor
      .cata(wrap) { rc =>
        logger.debug(s"<:> $rc")
        val currI = nextI
        val resolved =
          MakeRes
            .resolve(rc.topology.currentPeerId, currI)
            .lift
            .apply(rc.op)

        logger.trace("Resolved: " + resolved)

        if (debug) {
          println(Console.BLUE + rc + Console.RESET)
          println(currI + " : " + rc.topology)
          println("Before: " + rc.topology.beforeOn.value)
          println("Begin: " + rc.topology.beginsOn.value)
          println(
            (if (rc.topology.pathBefore.value.nonEmpty) Console.YELLOW
             else "") + "PathBefore: " + Console.RESET + rc.topology.pathBefore.value
          )

          println("Parent: " + Console.CYAN + rc.topology.parent.getOrElse("-") + Console.RESET)

          println("End  : " + rc.topology.endsOn.value)
          println("After: " + rc.topology.afterOn.value)
          println(
            "Exit : " + (if (rc.topology.forceExit.value) Console.MAGENTA + "true" + Console.RESET
                         else "false")
          )
          println(
            (if (rc.topology.pathAfter.value.nonEmpty) Console.YELLOW
             else "") + "PathAfter: " + Console.RESET + rc.topology.pathAfter.value
          )
          println(Console.YELLOW + "     -     -     -     -     -" + Console.RESET)
        }

        val chainZipperEv = resolved.traverse(cofree =>
          (
            rc.topology.pathBefore.map(through(_, s"before ${currI}")),
            rc.topology.pathAfter.map(through(_, s"after ${currI}", reversed = true))
          ).mapN { case (pathBefore, pathAfter) =>
            val cz = ChainZipper(
              pathBefore,
              cofree,
              pathAfter
            )
            if (cz.next.nonEmpty || cz.prev.nonEmpty) {
              logger.debug(s"Resolved   $rc -> $cofree")
              if (cz.prev.nonEmpty)
                logger.trace("From prev: " + cz.prev.map(_.head).toList.mkString(" -> "))
              if (cz.next.nonEmpty)
                logger.trace("To next:   " + cz.next.map(_.head).toList.mkString(" -> "))
            } else logger.debug(s"EMPTY    $rc -> $cofree")
            cz
          }
        )

        OptionT[Eval, ChainZipper[Res]](chainZipperEv)
      }

    logger.trace("Resolved Cofree: " + resolvedCofree.value.map(_.forceAll))

    resolvedCofree.map(NonEmptyChain.fromChain(_).map(_.uncons)).map {
      case None =>
        logger.error("Topology emitted nothing")
        SeqRes.leaf
      case Some((el, `nil`)) => el
      case Some((el, tail)) =>
        logger.warn("Topology emitted many nodes, that's unusual")
        SeqRes.wrap((el :: tail.toList): _*)
    }
  }

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(
    peerIds: Chain[ValueModel],
    log: String = null,
    reversed: Boolean = false
  ): Chain[Res] =
    peerIds.map { v =>
      v.`type` match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          FoldRes(itemName, v).wrap(
            if (reversed)
              SeqRes.wrap(
                NextRes(itemName).leaf,
                MakeRes.noop(VarModel(itemName, ScalarType.string, Chain.empty), log)
              )
            else
              SeqRes.wrap(
                MakeRes.noop(VarModel(itemName, ScalarType.string, Chain.empty), log),
                NextRes(itemName).leaf
              )
          )
        case _ =>
          MakeRes.noop(v, log)
      }
    }
}
