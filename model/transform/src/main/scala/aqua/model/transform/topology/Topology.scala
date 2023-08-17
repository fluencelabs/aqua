package aqua.model.transform.topology

import aqua.model.transform.topology.TopologyPath
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
import cats.kernel.Monoid
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
  val pathOn: Eval[TopologyPath] = Eval
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
  lazy val firstExecutesOn: Eval[Option[TopologyPath]] =
    (cursor.op match {
      case _: ForceExecModel => pathOn.map(_.some)
      case _ => children.collectFirstSomeM(_.firstExecutesOn)
    }).memoize

  // Find path of last `ForceExecModel` (call, canon, join) in this subtree
  lazy val lastExecutesOn: Eval[Option[TopologyPath]] =
    (cursor.op match {
      case _: ForceExecModel => pathOn.map(_.some)
      case _ => children.reverse.collectFirstSomeM(_.lastExecutesOn)
    }).memoize

  lazy val currentPeerId: Option[ValueModel] = pathOn.value.peerId

  // Get topology of previous sibling skipping `NoExec` nodes
  lazy val prevSibling: Option[Topology] = cursor.toPrevSibling.flatMap {
    // noExec nodes are meaningless topology-wise, so filter them out
    case c if c.isNoExec => c.topology.prevSibling
    case c => c.topology.some
  }

  // Get topology of next sibling skipping `NoExec` nodes
  lazy val nextSibling: Option[Topology] = cursor.toNextSibling.flatMap {
    // noExec nodes are meaningless topology-wise, so filter them out
    case c if c.isNoExec => c.topology.nextSibling
    case c => c.topology.some
  }

  // Get topology of first child skipping `NoExec` nodes
  lazy val firstChild: Option[Topology] = cursor.toFirstChild.flatMap {
    // noExec nodes are meaningless topology-wise, so filter them out
    case c if c.isNoExec => c.topology.nextSibling
    case c => c.topology.some
  }

  // Get topology of last child skipping `NoExec` nodes
  lazy val lastChild: Option[Topology] = cursor.toLastChild.flatMap {
    // noExec nodes are meaningless topology-wise, so filter them out
    case c if c.isNoExec => c.topology.prevSibling
    case c => c.topology.some
  }

  // Get children of current node skipping `NoExec` nodes
  lazy val children: LazyList[Topology] = cursor.children.filterNot(_.isNoExec).map(_.topology)

  // Find topologies of all nodes satisfying predicate in this subtree
  def findInside(f: Topology => Boolean): LazyList[Topology] =
    children.flatMap(_.findInside(f)).prependedAll(Option.when(f(this))(this))

  lazy val forModel: Option[ForModel] =
    Option(cursor.op).collect { case ft: ForModel => ft }

  lazy val isForModel: Boolean = forModel.isDefined

  // Before the left boundary of this element, what was the scope
  lazy val beforeOn: Eval[TopologyPath] = before.beforeOn(this).memoize

  // Inside the left boundary of this element, what should be the scope
  lazy val beginsOn: Eval[TopologyPath] = begins.beginsOn(this).memoize

  // After this element is done, what is the scope
  lazy val endsOn: Eval[TopologyPath] = ends.endsOn(this).memoize

  // After this element is done, where should it move to prepare for the next one
  lazy val afterOn: Eval[TopologyPath] = after.afterOn(this).memoize

  // Usually we don't care about exiting from where this tag ends into the outer scope
  // But for some cases, like par branches, its necessary, so the exit can be forced
  lazy val forceExit: Eval[Topology.ExitStrategy] = after.forceExit(this).memoize

  // Where we finally are, after exit enforcement is applied
  lazy val finallyOn: Eval[TopologyPath] = after.finallyOn(this).memoize

  lazy val pathBefore: Eval[Chain[ValueModel]] = begins.pathBefore(this).memoize

  lazy val pathAfter: Eval[Chain[ValueModel]] = after.pathAfter(this).memoize

  override def toString: String =
    s"Topology(${cursor},\n\tbefore: ${before},\n\tbegins: $begins,\n\tends: $ends,\n\tafter: $after)"
}

object Topology extends Logging {
  type Res = ResolvedOp.Tree

  enum ExitStrategy {
    case Full
    case ToRelay
    case Empty
  }

  object ExitStrategy {

    given Monoid[ExitStrategy] with {
      def empty: ExitStrategy = Empty

      def combine(x: ExitStrategy, y: ExitStrategy): ExitStrategy =
        (x, y) match {
          case (Full, _) | (_, Full) => Full
          case (ToRelay, _) | (_, ToRelay) => ToRelay
          case _ => Empty
        }
    }
  }

  def findRelayPathEnforcement(before: TopologyPath, begin: TopologyPath): Chain[ValueModel] =
    Chain.fromOption(
      // Get target peer of `begin`
      begin.peerId
        // Check that it is last relay of previous `on`
        .filter(lastPeerId => begin.previous.flatMap(_.lastRelay).contains(lastPeerId))
        // Check that it is not target peer of `before`
        .filterNot(lastPeerId => before.current.exists(_.peerId == lastPeerId))
    )

  // Return strategy for calculating `beforeOn` for
  // node pointed on by `cursor`
  private def decideBefore(cursor: OpModelTreeCursor): Before =
    cursor.parentOp match {
      case Some(XorModel) => XorBranch
      case Some(_: SeqGroupModel) => SeqGroupBranch
      case None => Root
      case _ => Default
    }

  // Return strategy for calculating `beginsOn` for
  // node pointed on by `cursor`
  private def decideBegins(cursor: OpModelTreeCursor): Begins =
    (cursor.parentOp, cursor.op) match {
      case (_, _: FailModel) => Fail
      case (Some(_: SeqGroupModel), _: NextModel) => SeqNext
      case (_, _: ForModel) => For
      // No begin optimization for detach
      case (_, ParModel) => ParGroup
      case _ => Default
    }

  // Return strategy for calculating `endsOn` for
  // node pointed on by `cursor`
  private def decideEnds(cursor: OpModelTreeCursor): Ends =
    cursor.op match {
      case _: SeqGroupModel => SeqGroup
      case XorModel => XorGroup
      case _: ParGroupModel => ParGroup
      case _ if cursor.parentOp.isEmpty => Root
      case _ => Default
    }

  // Return strategy for calculating `afterOn` for
  // node pointed on by `cursor`
  private def decideAfter(cursor: OpModelTreeCursor): After =
    (cursor.parentOp, cursor.op) match {
      case (_, _: FailModel) => Fail
      case (Some(_: ParGroupModel), _) => ParGroupBranch
      case (Some(XorModel), _) => XorBranch
      case (Some(_: SeqGroupModel), _) => SeqGroupBranch
      case (None, _) => Root
      case _ => Default
    }

  def make(cursor: OpModelTreeCursor): Topology =
    Topology(
      cursor = cursor,
      before = decideBefore(cursor),
      begins = decideBegins(cursor),
      ends = decideEnds(cursor),
      after = decideAfter(cursor)
    )

  def resolve(op: OpModel.Tree, debug: Boolean = false): Eval[Res] =
    resolveOnMoves(op, debug).flatMap(Cofree.cata(_) {
      case (SeqRes, children) =>
        Eval.later {
          children.uncons.collect {
            case (head, tail) if tail.isEmpty => head
          } getOrElse SeqRes.wrap(
            children.flatMap {
              case Cofree(SeqRes, ch) => ch.value
              case cf => Chain.one(cf)
            }
          )

        }
      case (head, children) => head.wrap(children).pure
    })

  def wrap(cz: ChainZipper[Res]): Chain[Res] =
    Chain.one(
      if (cz.prev.nonEmpty || cz.next.nonEmpty) SeqRes.wrap(cz.chain)
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

    val resolvedCofree = cursor.cata(wrap) { rc =>
      logger.debug(s"<:> $rc")
      val currI = nextI
      val resolved = MakeRes
        .resolve(rc.topology.currentPeerId, currI)
        .lift
        .apply(rc.op)

      logger.trace("Resolved: " + resolved)

      if (debug) printDebugInfo(rc, currI)

      val chainZipperEv = resolved.traverse(tree =>
        (
          rc.topology.pathBefore.map(through(_)),
          rc.topology.pathAfter.map(through(_, reversed = true))
        ).mapN { case (pathBefore, pathAfter) =>
          ChainZipper(pathBefore, tree, pathAfter)
        }.flatTap(logResolvedDebugInfo(rc, _, tree))
      )

      OptionT(chainZipperEv)
    }

    logger.trace("Resolved Cofree: " + resolvedCofree.value.map(_.forceAll))

    resolvedCofree.map(NonEmptyChain.fromChain(_).map(_.uncons)).map {
      case None =>
        logger.error("Topology emitted nothing")
        SeqRes.leaf
      case Some((el, `nil`)) => el
      case Some((el, tail)) =>
        logger.warn("Topology emitted many nodes, that's unusual")
        SeqRes.wrap(el :: tail.toList)
    }
  }

  // Walks through peer IDs, doing a noop function on each
  def through(
    peerIds: Chain[ValueModel],
    reversed: Boolean = false
  ): Chain[Res] = peerIds.map { v =>
    v.`type` match {
      case _: BoxType =>
        val itemName = "-via-peer-"
        val steps = Chain(
          MakeRes.hop(VarModel(itemName, ScalarType.string, Chain.empty)),
          NextRes(itemName).leaf
        )

        FoldRes(itemName, v).wrap(if (reversed) steps.reverse else steps)
      case _ =>
        MakeRes.hop(v)
    }
  }

  def printDebugInfo(rc: OpModelTreeCursor, i: Int): Unit = {
    println(Console.BLUE + rc + Console.RESET)
    println(i + " : " + rc.topology)
    println("Before: " + rc.topology.beforeOn.value.show)
    println("Begin: " + rc.topology.beginsOn.value.show)
    println(
      (if (rc.topology.pathBefore.value.nonEmpty) Console.YELLOW
       else "") + "PathBefore: " + Console.RESET + rc.topology.pathBefore.value
    )

    println("Parent: " + Console.CYAN + rc.topology.parent.getOrElse("-") + Console.RESET)

    println("End  : " + rc.topology.endsOn.value.show)
    println("After: " + rc.topology.afterOn.value.show)
    println("Exit : " + Console.MAGENTA + rc.topology.forceExit.value + Console.RESET)
    println(
      (if (rc.topology.pathAfter.value.nonEmpty) Console.YELLOW
       else "") + "PathAfter: " + Console.RESET + rc.topology.pathAfter.value
    )
    println(Console.YELLOW + "     -     -     -     -     -" + Console.RESET)
  }

  def logResolvedDebugInfo(
    rc: OpModelTreeCursor,
    cz: ChainZipper[ResolvedOp.Tree],
    tree: ResolvedOp.Tree
  ): Eval[Unit] =
    Eval.later {
      if (cz.next.nonEmpty || cz.prev.nonEmpty) {
        logger.debug(s"Resolved   $rc -> $tree")
        if (cz.prev.nonEmpty)
          logger.trace("From prev: " + cz.prev.map(_.head).toList.mkString(" -> "))
        if (cz.next.nonEmpty)
          logger.trace("To next:   " + cz.next.map(_.head).toList.mkString(" -> "))
      } else logger.debug(s"EMPTY    $rc -> $tree")
    }
}
