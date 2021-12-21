package aqua.model.transform.topology

import aqua.model.ValueModel.varName
import aqua.model.transform.cursor.ChainZipper
import aqua.model.func.raw.*
import aqua.model.transform.res.*
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain.{==:, nil}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse.*
import scribe.Logging

case class Topology(
  cursor: RawCursor,
  before: Topology.Before,
  begins: Topology.Begins,
  ends: Topology.Ends,
  after: Topology.After
) {

  final lazy val pathOn: List[OnTag] = cursor.tagsPath.collect { case o: OnTag =>
    o
  }

  final lazy val currentPeerId: Option[ValueModel] = pathOn.headOption.map(_.peerId)

  final lazy val prevSibling: Option[Topology] = cursor.toPrevSibling.map(_.topology)

  final lazy val nextSibling: Option[Topology] = cursor.toNextSibling.map(_.topology)

  final lazy val firstChild: Option[Topology] = cursor.toFirstChild.map(_.topology)

  final lazy val lastChild: Option[Topology] = cursor.toLastChild.map(_.topology)

  final val parent: Option[Topology] = cursor.moveUp.map(_.topology)

  final val parents: LazyList[Topology] =
    LazyList.unfold(parent)(p => p.map(pp => pp -> pp.parent))

  lazy val forTag: Option[ForTag] = Option(cursor.tag).collect { case ft: ForTag =>
    ft
  }

  lazy val isForTag: Boolean = forTag.isDefined

  // Before the left boundary of this element, what was the scope
  lazy val beforeOn: List[OnTag] = before.beforeOn(this)

  // Inside the left boundary of this element, what should be the scope
  lazy val beginsOn: List[OnTag] = begins.beginsOn(this)

  // After this element is done, what is the scope
  lazy val endsOn: List[OnTag] = ends.endsOn(this)

  // After this element is done, where should it move to prepare for the next one
  lazy val afterOn: List[OnTag] = after.afterOn(this)

  lazy val forceExit: Boolean = after.forceExit(this)

  // Where we finnaly are
  lazy val finallyOn: List[OnTag] = after.finallyOn(this)

  lazy val pathBefore: Chain[ValueModel] = begins.pathBefore(this)

  lazy val pathAfter: Chain[ValueModel] =
    after.pathAfter(this)
}

object Topology extends Logging {
  type Tree = Cofree[Chain, RawTag]
  type Res = Cofree[Chain, ResolvedOp]

  trait Before {

    def beforeOn(current: Topology): List[OnTag] =
      // Go to the parent, see where it begins
      current.parent.map(_.beginsOn) getOrElse
        // This means, we have no parent; then we're where we should be
        current.pathOn
  }

  trait Begins {
    def beginsOn(current: Topology): List[OnTag] = current.pathOn

    def pathBefore(current: Topology): Chain[ValueModel] =
      PathFinder.findPath(current.beforeOn, current.beginsOn)
  }

  trait Ends {

    def endsOn(current: Topology): List[OnTag] =
      current.beginsOn

    // TODO 1
    protected def lastChildFinally(current: Topology): List[OnTag] =
      current.lastChild.map {
        case lc if lc.forceExit => current.afterOn
        case lc => lc.endsOn
      } getOrElse current.beginsOn
  }

  trait After {
    def forceExit(current: Topology): Boolean = false

    def afterOn(current: Topology): List[OnTag] = current.endsOn

    // TODO 3
    protected def afterParent(current: Topology): List[OnTag] =
      current.parent.map(
        _.afterOn
      ) getOrElse current.pathOn

    final def finallyOn(current: Topology): List[OnTag] =
      if (current.forceExit) current.afterOn else current.endsOn

    def pathAfter(current: Topology): Chain[ValueModel] =
      if (current.forceExit) PathFinder.findPath(current.endsOn, current.afterOn) else Chain.empty
  }

  object Default extends Before with Begins with Ends with After {
    override def toString: String = "<default>"
  }

  // Parent == Seq, On
  object SeqGroupBranch extends Before with After {
    override def toString: String = "<seq>/*"

    override def beforeOn(current: Topology): List[OnTag] =
      current.prevSibling
        .map(_.finallyOn) getOrElse super.beforeOn(current)

    // TODO 2
    override def afterOn(current: Topology): List[OnTag] =
      current.nextSibling.map(_.beginsOn) getOrElse afterParent(current)

  }

  object SeqGroup extends Ends {
    override def toString: String = "<seq>"

    override def endsOn(current: Topology): List[OnTag] =
      lastChildFinally(current)
  }

  // Parent == Xor
  object XorBranch extends Before with After {
    override def toString: String = "<xor>/*"

    override def beforeOn(current: Topology): List[OnTag] =
      current.prevSibling.map(_.endsOn) getOrElse super.beforeOn(current)

    // TODO: if this xor is in par that needs no forceExit, do not exit
    override def forceExit(current: Topology): Boolean =
      current.cursor.moveUp.exists(_.hasExecLater)

    override def afterOn(current: Topology): List[OnTag] =
      afterParent(current)
  }

  // Parent == Par
  object ParBranch extends Ends with After {
    override def toString: String = "<par>/*"

    override def forceExit(current: Topology): Boolean = current.cursor.exportsUsedLater

    override def afterOn(current: Topology): List[OnTag] =
      afterParent(current)

    override def pathAfter(current: Topology): Chain[ValueModel] = {
      val pa = super.pathAfter(current)
      // Ping the next (join) peer to enforce its data update
      if (pa.nonEmpty) pa ++ Chain.fromOption(current.afterOn.headOption.map(_.peerId)) else pa
    }

    override def endsOn(current: Topology): List[OnTag] = current.beforeOn
  }

  object XorGroup extends Ends {
    override def toString: String = "<xor>"

    // Xor tag ends where any child ends; can't get first one as it may lead to recursion
    override def endsOn(current: Topology): List[OnTag] =
      lastChildFinally(current)

  }

  object Root extends Before with Ends with After {
    override def toString: String = "<root>"

    override def beforeOn(current: Topology): List[OnTag] = current.beginsOn

    override def endsOn(current: Topology): List[OnTag] = current.pathOn

    override def afterOn(current: Topology): List[OnTag] = current.pathOn

    override def forceExit(current: Topology): Boolean = false
  }

  object For extends Begins {
    override def toString: String = "<for>"

    override def beginsOn(current: Topology): List[OnTag] =
      (current.forTag zip current.firstChild.map(_.beginsOn)).map { case (f, b) =>
        // Take path until this for's iterator is used
        b.reverse
          .foldLeft((true, List.empty[OnTag])) {
            case ((true, acc), OnTag(_, r)) if r.exists(ValueModel.varName(_).contains(f.item)) =>
              (false, acc)
            case ((true, acc @ (OnTag(_, r @ (r0 ==: _)) :: _)), OnTag(p, _))
                if ValueModel.varName(p).contains(f.item) =>
              // This is to take the outstanding relay and force moving there
              (false, OnTag(r0, r) :: acc)
            case ((true, acc), on) => (true, on :: acc)
            case ((false, acc), _) => (false, acc)
          }
          ._2
      } getOrElse super.beginsOn(current)

    override def pathBefore(current: Topology): Chain[ValueModel] =
      PathFinder.findPath(
        current.beforeOn,
        current.beginsOn match {
          case OnTag(z, relays @ y ==: _) :: tail if z == y =>
            OnTag(LiteralModel.quote("impossible peer"), relays) :: tail
          case path => path
        }
      )

  }

  object SeqNext extends Begins {
    override def toString: String = "<seq>/<next>"

    override def beginsOn(current: Topology): List[OnTag] =
      current.parents.find(_.isForTag).map(_.beginsOn) getOrElse super.beginsOn(current)
  }

  def make(cursor: RawCursor): Topology =
    Topology(
      cursor,
      // Before
      cursor.parentTag match {
        case Some(XorTag) => XorBranch
        case Some(_: SeqGroupTag) => SeqGroupBranch
        case None => Root
        case _ => Default
      },
      // Begin
      (cursor.parentTag, cursor.tag) match {
        case (Some(_: SeqGroupTag), _: NextTag) =>
          SeqNext
        case (_, _: ForTag) =>
          For
        case _ =>
          Default
      },
      // End
      cursor.tag match {
        case _: SeqGroupTag => SeqGroup
        case XorTag => XorGroup
        case _ if cursor.parentTag.isEmpty => Root
        case _ => Default
      },
      // After
      cursor.parentTag match {
        case Some(ParTag) => ParBranch
        case Some(XorTag) => XorBranch
        case Some(_: SeqGroupTag) => SeqGroupBranch
        case None => Root
        case _ => Default
      }
    )

  def resolve(op: Tree, debug: Boolean = false): Res = {
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
      .value
  }

  def wrap(cz: ChainZipper[Res]): Chain[Res] =
    Chain.one(
      if (cz.prev.nonEmpty || cz.next.nonEmpty) Cofree(SeqRes, Eval.now(cz.chain))
      else cz.current
    )

  def resolveOnMoves(op: Tree, debug: Boolean): Eval[Res] = {
    val cursor = RawCursor(NonEmptyList.one(ChainZipper.one(op)), None)
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
            .apply(rc.tag)

        logger.trace("Resolved: " + resolved)

        if (debug) {
          println(Console.BLUE + rc + Console.RESET)
          println(rc.topology)
          println("Before: " + rc.topology.beforeOn)
          println("Begin: " + rc.topology.beginsOn)
          println("PathBefore: " + rc.topology.pathBefore)
        }

        val chainZipperEv = resolved.traverse(cofree =>
          Eval.later {
            val cz = ChainZipper(
              through(rc.topology.pathBefore),
              cofree,
              through(rc.topology.pathAfter, reversed = true)
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
      v.lastType match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          MakeRes.fold(
            itemName,
            v,
            if (reversed)
              MakeRes.seq(
                MakeRes.next(itemName),
                MakeRes.noop(VarModel(itemName, ScalarType.string))
              )
            else
              MakeRes.seq(
                MakeRes.noop(VarModel(itemName, ScalarType.string)),
                MakeRes.next(itemName)
              )
          )
        case _ =>
          MakeRes.noop(v)
      }
    }
}
