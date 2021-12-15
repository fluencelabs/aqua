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

sealed abstract class Topology(forceExit: Boolean = false) {

  def cursor: RawCursor

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

  def isForTag: Boolean = cursor.tag match {
    case _: ForTag => true
    case _ => false
  }

  final def parent: Option[Topology] = cursor.moveUp.map(_.topology)

  final def parents: LazyList[Topology] =
    LazyList.unfold(parent)(p => p.map(pp => pp -> pp.parent))

  // Before the left boundary of this element, what was the scope
  def beforeOn: List[OnTag] =
    // Go to the parent, see where it begins
    parent.map(_.beginsOn) getOrElse
      // This means, we have no parent; then we're where we should be
      beginsOn

  // Inside the left boundary of this element, what should be the scope
  def beginsOn: List[OnTag] = pathOn

  // After this element is done, what is the scope
  def endsOn: List[OnTag] = beginsOn

  // After this element is done, where should it move to prepare for the next one
  def afterOn: List[OnTag] = endsOn

  // Where we finnaly are
  final def finallyOn: List[OnTag] = if (forceExit) afterOn else endsOn

  final def pathBefore: Chain[ValueModel] = PathFinder.findPath(beforeOn, beginsOn)

  def pathAfter: Chain[ValueModel] =
    if (forceExit) PathFinder.findPath(endsOn, afterOn) else Chain.empty
}

object Topology extends Logging {
  type Tree = Cofree[Chain, RawTag]
  type Res = Cofree[Chain, ResolvedOp]

  // Parent == Xor
  case class XorBranch(cursor: RawCursor)
      extends Topology(forceExit = cursor.moveUp.exists(_.hasExecLater)) {

    override def beforeOn: List[OnTag] =
      prevSibling.map(_.endsOn) getOrElse super.beforeOn

    override def afterOn: List[OnTag] =
      parent.flatMap(_.nextSibling).map(_.beginsOn) orElse parent.map(
        _.afterOn
      ) getOrElse super.afterOn
  }

  case class ParBranch(cursor: RawCursor) extends Topology(forceExit = cursor.exportsUsedLater) {

    override def afterOn: List[OnTag] =
      parent.flatMap(_.nextSibling).map(_.beginsOn) orElse parent.map(
        _.afterOn
      ) getOrElse super.afterOn

    override def pathAfter: Chain[ValueModel] = {
      val pa = super.pathAfter
      // Ping the next (join) peer to enforce its data update
      if (pa.nonEmpty) pa ++ Chain.fromOption(afterOn.headOption.map(_.peerId)) else pa
    }
  }

  // Parent == Seq, On
  case class SeqGroupBranch(cursor: RawCursor) extends Topology() {

    override def beforeOn: List[OnTag] =
      prevSibling
        .map(_.finallyOn) getOrElse super.beforeOn

    override def beginsOn: List[OnTag] =
      if (isNextTag)
        parents.find(_.isForTag).map(_.beginsOn) getOrElse super.beginsOn
      else
        super.beginsOn

    override def endsOn: List[OnTag] =
      cursor.toLastChild
        .map(_.topology)
        .map(_.finallyOn) getOrElse super.endsOn

    override def afterOn: List[OnTag] =
      nextSibling.map(_.beginsOn) orElse parent.map(_.afterOn) getOrElse super.afterOn
  }

  case class Default(cursor: RawCursor) extends Topology()
  case class Next(cursor: RawCursor) extends Topology()

  // Branch contains no executable instructions -- no need for topology
  case class NoExec(cursor: RawCursor) extends Topology() {
    override def beginsOn: List[OnTag] = super.beforeOn

    override def endsOn: List[OnTag] = beginsOn
  }

  def apply(cursor: RawCursor): Topology = cursor.parentTag match {
    case _ if cursor.isNoExec => NoExec(cursor)
    case Some(XorTag) =>
      XorBranch(cursor)
    case Some(ParTag) =>
      ParBranch(cursor)
    case Some(_: SeqGroupTag) =>
      SeqGroupBranch(cursor)
    case _ => Default(cursor)
  }

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
              through(rc.topology.pathAfter)
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
