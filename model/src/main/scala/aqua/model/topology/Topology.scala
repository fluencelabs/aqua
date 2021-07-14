package aqua.model.topology

import aqua.model.cursor.ChainZipper
import aqua.model.func.raw._
import aqua.model.func.resolved._
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain.nil
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse._
import wvlet.log.LogSupport

object Topology extends LogSupport {
  type Tree = Cofree[Chain, RawTag]
  type Res = Cofree[Chain, ResolvedOp]

  def resolve(op: Tree): Res = {
    val resolved = resolveOnMoves(op).value
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

  private def rawToResolved(
    currentPeerId: Option[ValueModel]
  ): PartialFunction[RawTag, ResolvedOp] = {
    case SeqTag => SeqRes
    case _: OnTag => SeqRes
    case MatchMismatchTag(a, b, s) => MatchMismatchRes(a, b, s)
    case ForTag(item, iter) => FoldRes(item, iter)
    case ParTag | ParTag.Detach => ParRes
    case XorTag | XorTag.LeftBiased => XorRes
    case NextTag(item) => NextRes(item)
    case CallServiceTag(serviceId, funcName, call) =>
      CallServiceRes(
        serviceId,
        funcName,
        call,
        currentPeerId
          .getOrElse(LiteralModel.initPeerId)
      )
  }

  def resolveOnMoves(op: Tree): Eval[Res] = {
    val cursor = RawCursor(NonEmptyList.one(ChainZipper.one(op)))
    val resolvedCofree = cursor
      .cata(wrap) { rc =>
        debug(s"<:> $rc")
        val resolved = rawToResolved(rc.currentPeerId).lift
          .apply(rc.tag)
          .map(MakeRes.leaf)
        val chainZipperEv = resolved.traverse(cofree =>
          Eval.later {
            val cz = ChainZipper(
              through(rc.pathFromPrev),
              cofree,
              through(rc.pathToNext)
            )
            if (cz.next.nonEmpty || cz.prev.nonEmpty) {
              debug(s"Resolved   $rc -> $cofree")
              if (cz.prev.nonEmpty)
                trace("From prev: " + cz.prev.map(_.head).toList.mkString(" -> "))
              if (cz.next.nonEmpty)
                trace("To next:   " + cz.next.map(_.head).toList.mkString(" -> "))
            } else debug(s"EMPTY    $rc -> $cofree")
            cz
          }
        )
        OptionT[Eval, ChainZipper[Res]](chainZipperEv)
      }

    resolvedCofree.map(NonEmptyChain.fromChain(_).map(_.uncons)).map {
      case None =>
        error("Topology emitted nothing")
        Cofree(SeqRes, MakeRes.nilTail)
      case Some((el, `nil`)) => el
      case Some((el, tail)) =>
        warn("Topology emitted many nodes, that's unusual")
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
