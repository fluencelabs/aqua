package aqua.model.transform.topology

import aqua.model.transform.cursor.ChainZipper
import aqua.model.func.raw.*
import aqua.model.transform.res.*
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain.nil
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

  final def currentPeerId: Option[ValueModel] = pathOn.headOption.map(_.peerId)

  final def prevSibling: Option[Topology] = cursor.toPrevSibling.map(_.topology)
  final def nextSibling: Option[Topology] = cursor.toNextSibling.map(_.topology)

  def isNextTag: Boolean = cursor.tag match {
    case _: NextTag => true
    case _ => false
  }

  def forTag: Option[ForTag] = Option(cursor.tag).collect { case ft: ForTag =>
    ft
  }

  def isForTag: Boolean = forTag.isDefined

  final def parent: Option[Topology] = cursor.moveUp.map(_.topology)

  final def parents: LazyList[Topology] =
    LazyList.unfold(parent)(p => p.map(pp => pp -> pp.parent))

  // Before the left boundary of this element, what was the scope
  def beforeOn: List[OnTag] = before.beforeOn(this)

  // Inside the left boundary of this element, what should be the scope
  def beginsOn: List[OnTag] = begins.beginsOn(this)

  // After this element is done, what is the scope
  def endsOn: List[OnTag] = ends.endsOn(this)

  // After this element is done, where should it move to prepare for the next one
  def afterOn: List[OnTag] = after.afterOn(this)

  // Where we finnaly are
  final def finallyOn: List[OnTag] = after.finallyOn(this)

  final def pathBefore: Chain[ValueModel] = PathFinder.findPath(beforeOn, beginsOn)

  def pathAfter: Chain[ValueModel] =
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
  }

  trait After {
    def forceExit(current: Topology): Boolean = false

    def afterOn(current: Topology): List[OnTag] = current.endsOn

    final def finallyOn(current: Topology): List[OnTag] =
      if (forceExit(current)) current.afterOn else current.endsOn

    def pathAfter(current: Topology): Chain[ValueModel] =
      if (forceExit(current)) PathFinder.findPath(current.endsOn, current.afterOn) else Chain.empty
  }

  trait Ends {

    def endsOn(current: Topology): List[OnTag] =
      current.beginsOn
  }

  object Default extends Before with Begins with Ends with After

  // Parent == Seq, On
  object SeqGroupBranch extends Before with Begins with After {

    override def beforeOn(current: Topology): List[OnTag] =
      current.prevSibling
        .map(_.finallyOn) getOrElse super.beforeOn(current)

    override def beginsOn(current: Topology): List[OnTag] =
      if (current.isNextTag)
        current.parents.find(_.isForTag).map(_.beginsOn) getOrElse super.beginsOn(current)
      else
        super.beginsOn(current)

    override def afterOn(current: Topology): List[OnTag] =
      current.nextSibling.map(_.beginsOn) orElse current.parent.map(_.afterOn) getOrElse super
        .afterOn(current)

  }

  object SeqGroup extends Ends {

    override def endsOn(current: Topology): List[OnTag] =
      current.cursor.toLastChild
        .map(_.topology)
        .map(_.finallyOn) getOrElse current.beginsOn
  }

  // Parent == Xor
  object XorBranch extends Before with After {

    override def beforeOn(current: Topology): List[OnTag] =
      current.prevSibling.map(_.endsOn) getOrElse super.beforeOn(current)

    // TODO: if this xor is in par that needs no forceExit, do not exit
    override def forceExit(current: Topology): Boolean =
      current.cursor.moveUp.exists(_.hasExecLater)

    override def afterOn(current: Topology): List[OnTag] =
      current.parent.map(
        _.afterOn
      ) getOrElse super.afterOn(current)
  }

  // Parent == Par
  object ParBranch extends Ends with After {
    override def forceExit(current: Topology): Boolean = current.cursor.exportsUsedLater

    override def afterOn(current: Topology): List[OnTag] =
      current.parent.map(
        _.afterOn
      ) getOrElse super.afterOn(current)

    override def pathAfter(current: Topology): Chain[ValueModel] = {
      val pa = super.pathAfter(current)
      // Ping the next (join) peer to enforce its data update
      if (pa.nonEmpty) pa ++ Chain.fromOption(afterOn(current).headOption.map(_.peerId)) else pa
    }

    override def endsOn(current: Topology): List[OnTag] = current.beforeOn
  }

  // No need to move anywhere
  object NoExecItem extends Begins with Ends {
    override def beginsOn(current: Topology): List[OnTag] = current.beforeOn

    override def endsOn(current: Topology): List[OnTag] = current.beginsOn
  }

  object XorGroup extends Ends {
    // Xor tag ends where any child ends; can't get first one as it may lead to recursion
    override def endsOn(current: Topology): List[OnTag] =
      current.cursor.toLastChild
        .map(_.topology)
        .map(_.finallyOn) getOrElse super.endsOn(current)
  }

  def make(cursor: RawCursor): Topology =
    Topology(
      cursor,
      // Before
      cursor.parentTag match {
        case Some(XorTag) => XorBranch
        case Some(_: SeqGroupTag) => SeqGroupBranch
        case _ => Default
      },
      // Begin
      cursor.parentTag match {
        case _ if cursor.isNoExec => NoExecItem
        case Some(_: SeqGroupTag) => SeqGroupBranch
        case _ => Default
      },
      // End
      cursor.tag match {
        case _ if cursor.isNoExec => NoExecItem
        case _: SeqGroupTag => SeqGroup
        case XorTag => XorGroup
        case _ => Default
      },
      // After
      cursor.parentTag match {
        case Some(ParTag) => ParBranch
        case Some(XorTag) => XorBranch
        case Some(_: SeqGroupTag) => SeqGroupBranch
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
