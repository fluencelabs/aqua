package aqua.model.transform.topology

import aqua.model.transform.cursor.ChainZipper
import aqua.model.transform.res.*
import aqua.model.{
  CallServiceModel,
  ForModel,
  LiteralModel,
  NextModel,
  OnModel,
  OpModel,
  ParGroupModel,
  ParModel,
  SeqGroupModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain.{==:, nil}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse.*
import cats.syntax.apply.*
import scribe.Logging

/**
 * Wraps all the logic for topology reasoning about the tag in the AST represented by the [[cursor]]
 *
 * @param cursor
 *   Pointer to the current place in the AST
 * @param before
 *   Strategy of calculating where the previous executions happened
 * @param begins
 *   Strategy of calculating where execution of this tag/node should begin
 * @param ends
 *   Strategy of calculating where execution of this tag/node happens
 * @param after
 *   Strategy of calculating where the next execution should happen and whether we need to move
 *   there or not
 */
case class Topology private (
  cursor: OpModelTreeCursor,
  before: Topology.Before,
  begins: Topology.Begins,
  ends: Topology.Ends,
  after: Topology.After
) {

  val pathOn: Eval[List[OnModel]] = Eval
    .later(cursor.tagsPath.collect { case o: OnModel =>
      o
    })
    .memoize

  lazy val firstExecutesOn: Eval[Option[List[OnModel]]] =
    (cursor.op match {
      case _: CallServiceModel => pathOn.map(Some(_))
      case _ =>
        children
          .map(_.firstExecutesOn)
          .scanLeft[Eval[Option[List[OnModel]]]](Eval.now(None)) { case (acc, el) =>
            (acc, el).mapN(_ orElse _)
          }
          .collectFirst {
            case e if e.value.isDefined => e
          }
          .getOrElse(Eval.now(None))
    }).memoize

  lazy val lastExecutesOn: Eval[Option[List[OnModel]]] =
    (cursor.op match {
      case _: CallServiceModel => pathOn.map(Some(_))
      case _ =>
        children
          .map(_.lastExecutesOn)
          .scanRight[Eval[Option[List[OnModel]]]](Eval.now(None)) { case (acc, el) =>
            (acc, el).mapN(_ orElse _)
          }
          .collectFirst {
            case e if e.value.isDefined => e
          }
          .getOrElse(Eval.now(None))
    }).memoize

  lazy val currentPeerId: Option[ValueModel] = pathOn.value.headOption.map(_.peerId)

  lazy val prevSibling: Option[Topology] = cursor.toPrevSibling.map(_.topology)

  lazy val nextSibling: Option[Topology] = cursor.toNextSibling.map(_.topology)

  lazy val firstChild: Option[Topology] = cursor.toFirstChild.map(_.topology)

  lazy val lastChild: Option[Topology] = cursor.toLastChild.map(_.topology)

  lazy val children: LazyList[Topology] = cursor.children.map(_.topology)

  def findInside(f: Topology => Boolean): LazyList[Topology] =
    children.flatMap(_.findInside(f)).prependedAll(Option.when(f(this))(this))

  val parent: Option[Topology] = cursor.moveUp.map(_.topology)

  val parents: LazyList[Topology] =
    LazyList.unfold(parent)(p => p.map(pp => pp -> pp.parent))

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
}

object Topology extends Logging {
  type Res = ResolvedOp.Tree

  // Returns a peerId to go to in case it equals the last relay: useful when we do execution on the relay
  private def findRelayPathEnforcement(bef: List[OnModel], beg: List[OnModel]): Chain[ValueModel] =
    Chain.fromOption(
      beg.headOption
        .map(_.peerId)
        .filter(lastPeerId => beg.tail.headOption.exists(_.via.lastOption.contains(lastPeerId)))
        .filter(lastPeerId => !bef.headOption.exists(_.peerId == lastPeerId))
    )

  trait Before {

    def beforeOn(current: Topology): Eval[List[OnModel]] =
      // Go to the parent, see where it begins
      current.parent.map(_.beginsOn) getOrElse
        // This means, we have no parent; then we're where we should be
        current.pathOn
  }

  trait Begins {

    def beginsOn(current: Topology): Eval[List[OnModel]] = current.pathOn

    def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
      (current.beforeOn, current.beginsOn).mapN { case (bef, beg) =>
        (PathFinder.findPath(bef, beg), bef, beg)
      }.flatMap { case (pb, bef, beg) =>
        // Handle the case when we need to go through the relay, but miss the hop as it's the first
        // peer where we go, but there's no service calls there
        current.firstExecutesOn.map {
          case Some(where) if where != beg =>
            pb ++ findRelayPathEnforcement(bef, beg)
          case _ => pb
        }
      }
  }

  trait Ends {

    def endsOn(current: Topology): Eval[List[OnModel]] =
      current.beginsOn

    protected def lastChildFinally(current: Topology): Eval[List[OnModel]] =
      current.lastChild.map(lc =>
        lc.forceExit.flatMap {
          case true => current.afterOn
          case false => lc.endsOn
        }
      ) getOrElse current.beginsOn
  }

  trait After {
    def forceExit(current: Topology): Eval[Boolean] = Eval.now(false)

    def afterOn(current: Topology): Eval[List[OnModel]] = current.pathOn

    protected def afterParent(current: Topology): Eval[List[OnModel]] =
      current.parent.map(
        _.afterOn
      ) getOrElse current.pathOn

    // In case exit is performed and pathAfter is inserted, we're actually where
    // execution is expected to continue After this node is handled
    final def finallyOn(current: Topology): Eval[List[OnModel]] =
      current.forceExit.flatMap {
        case true => current.afterOn
        case false => current.endsOn
      }

    // If exit is forced, make a path outside this node
    // – from where it ends to where execution is expected to continue
    def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
      current.forceExit.flatMap {
        case true =>
          (current.endsOn, current.afterOn).mapN(PathFinder.findPath)
        case false =>
          Eval.now(Chain.empty)
      }
  }

  object Default extends Before with Begins with Ends with After {
    override def toString: String = "<default>"
  }

  // Parent == Seq, On
  object SeqGroupBranch extends Before with After {
    override def toString: String = "<seq>/*"

    // If parent is seq, then before this node we are where previous node, if any, ends
    override def beforeOn(current: Topology): Eval[List[OnModel]] =
      current.prevSibling
        .map(_.finallyOn) getOrElse super.beforeOn(current)

    override def afterOn(current: Topology): Eval[List[OnModel]] =
      current.nextSibling.map(_.beginsOn) getOrElse afterParent(current)

  }

  object SeqGroup extends Ends {
    override def toString: String = "<seq>"

    override def endsOn(current: Topology): Eval[List[OnModel]] =
      lastChildFinally(current)
  }

  // Parent == Xor
  object XorBranch extends Before with After {
    override def toString: String = "<xor>/*"

    override def beforeOn(current: Topology): Eval[List[OnModel]] =
      current.prevSibling.map(_.endsOn) getOrElse super.beforeOn(current)

    // TODO: if this xor is in par that needs no forceExit, do not exit
    override def forceExit(current: Topology): Eval[Boolean] =
      Eval.later(current.cursor.moveUp.exists(_.hasExecLater))

    override def afterOn(current: Topology): Eval[List[OnModel]] =
      afterParent(current)
  }

  // Parent == Par
  object ParGroupBranch extends Ends with After {
    override def toString: String = "<par>/*"

    override def forceExit(current: Topology): Eval[Boolean] =
      Eval.later(current.cursor.exportsUsedLater)

    override def afterOn(current: Topology): Eval[List[OnModel]] =
      afterParent(current)

    override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
      current.forceExit
        .flatMap[Chain[ValueModel]] {
          case false => Eval.now(Chain.empty[ValueModel])
          case true =>
            (current.endsOn, current.afterOn, current.lastExecutesOn).mapN {
              case (e, a, _) if e == a => Chain.empty[ValueModel]
              case (e, a, l) if l.contains(e) =>
                // Pingback in case no relays involved
                Chain.fromOption(a.headOption.map(_.peerId))
              case (e, a, _) =>
                // We wasn't at e, so need to get through the last peer in case it matches with the relay
                findRelayPathEnforcement(a, e) ++ Chain.fromOption(a.headOption.map(_.peerId))
            }
        }
        .flatMap { appendix =>
          // Ping the next (join) peer to enforce its data update
          super.pathAfter(current).map(_ ++ appendix)
        }

    override def endsOn(current: Topology): Eval[List[OnModel]] = current.beforeOn
  }

  object XorGroup extends Ends {
    override def toString: String = "<xor>"

    // Xor tag ends where any child ends; can't get first one as it may lead to recursion
    override def endsOn(current: Topology): Eval[List[OnModel]] =
      lastChildFinally(current)

  }

  object Root extends Before with Ends with After {
    override def toString: String = "<root>"

    override def beforeOn(current: Topology): Eval[List[OnModel]] = current.beginsOn

    override def endsOn(current: Topology): Eval[List[OnModel]] = current.pathOn

    override def afterOn(current: Topology): Eval[List[OnModel]] = current.pathOn

    override def forceExit(current: Topology): Eval[Boolean] = Eval.now(false)
  }

  object ParGroup extends Begins with Ends {
    override def toString: String = "<par>"

    // Optimization: find the longest common prefix of all the par branches, and move it outside of this par
    // When branches will calculate their paths, they will take this move into account.
    // So less hops will be produced
    override def beginsOn(current: Topology): Eval[List[OnModel]] =
      current.children
        .map(_.beginsOn.map(_.reverse))
        .reduceLeftOption { case (b1e, b2e) =>
          (b1e, b2e).mapN { case (b1, b2) =>
            (b1 zip b2).takeWhile(_ == _).map(_._1)
          }
        }
        .map(_.map(_.reverse)) getOrElse super.beginsOn(current)

    // Par block ends where all the branches end, if they have forced exit (not fire-and-forget)
    override def endsOn(current: Topology): Eval[List[OnModel]] =
      current.children
        .map(_.forceExit)
        .reduceLeftOption { case (a, b) =>
          (a, b).mapN(_ || _)
        }
        .map(_.flatMap {
          case true => current.afterOn
          case false => super.endsOn(current)
        }) getOrElse super.endsOn(current)
  }

  object For extends Begins {
    override def toString: String = "<for>"

    // Optimization: get all the path inside the For block out of the block, to avoid repeating
    // hops for every For iteration
    override def beginsOn(current: Topology): Eval[List[OnModel]] =
      (current.forModel zip current.firstChild.map(_.beginsOn)).map { case (f, b) =>
        // Take path until this for's iterator is used
        b.map(
          _.reverse
            .foldLeft((true, List.empty[OnModel])) {
              case ((true, acc), OnModel(_, r)) if r.exists(_.usesVarNames.contains(f.item)) =>
                (false, acc)
              case ((true, acc @ (OnModel(_, r @ (r0 ==: _)) :: _)), OnModel(p, _))
                  if p.usesVarNames.contains(f.item) =>
                // This is to take the outstanding relay and force moving there
                (false, OnModel(r0, r) :: acc)
              case ((true, acc), on) => (true, on :: acc)
              case ((false, acc), _) => (false, acc)
            }
            ._2
        )
      } getOrElse super.beginsOn(current)

  }

  object SeqNext extends Begins {
    override def toString: String = "<seq>/<next>"

    override def beginsOn(current: Topology): Eval[List[OnModel]] =
      current.parents.find(_.isForModel).map(_.beginsOn) getOrElse super.beginsOn(current)
  }

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

  def resolve(op: OpModel.Tree, debug: Boolean = false): Eval[Res] = {
    val resolved = resolveOnMoves(op, debug).value
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
  }

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
        val resolved =
          MakeRes
            .resolve(rc.topology.currentPeerId, nextI)
            .lift
            .apply(rc.op)

        logger.trace("Resolved: " + resolved)

        if (debug) {
          println(Console.BLUE + rc + Console.RESET)
          println(rc.topology)
          println("Before: " + rc.topology.beforeOn.value)
          println("Begin: " + rc.topology.beginsOn.value)
          println(
            (if (rc.topology.pathBefore.value.nonEmpty) Console.YELLOW
             else "") + "PathBefore: " + Console.RESET + rc.topology.pathBefore.value
          )

          println(Console.CYAN + "Parent: " + rc.topology.parent + Console.RESET)

          println("End  : " + rc.topology.endsOn.value)
          println("After: " + rc.topology.afterOn.value)
          println("Exit : " + rc.topology.forceExit.value)
          println(
            (if (rc.topology.pathAfter.value.nonEmpty) Console.YELLOW
             else "") + "PathAfter: " + Console.RESET + rc.topology.pathAfter.value
          )
          println(Console.YELLOW + "     -     -     -     -     -" + Console.RESET)
        }

        val chainZipperEv = resolved.traverse(cofree =>
          (
            rc.topology.pathBefore.map(through(_)),
            rc.topology.pathAfter.map(through(_, reversed = true))
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
        Cofree(SeqRes, MakeRes.nilTail)
      case Some((el, `nil`)) => el
      case Some((el, tail)) =>
        logger.warn("Topology emitted many nodes, that's unusual")
        Cofree(SeqRes, Eval.now(el +: tail))
    }
  }

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(peerIds: Chain[ValueModel], reversed: Boolean = false): Chain[Res] =
    peerIds.map { v =>
      v.`type` match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          MakeRes.fold(
            itemName,
            v,
            if (reversed)
              MakeRes.seq(
                MakeRes.next(itemName),
                MakeRes.noop(VarModel(itemName, ScalarType.string, Chain.empty))
              )
            else
              MakeRes.seq(
                MakeRes.noop(VarModel(itemName, ScalarType.string, Chain.empty)),
                MakeRes.next(itemName)
              )
          )
        case _ =>
          MakeRes.noop(v)
      }
    }
}
