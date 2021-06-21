package aqua.model.topology

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.model.func.raw._
import cats.Eval
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.data.Chain.nil
import cats.free.Cofree
import aqua.model.cursor.ChainZipper
import aqua.model.func.resolved._
import aqua.types.{BoxType, ScalarType}
import wvlet.log.LogSupport
import cats.syntax.traverse._

object Topology extends LogSupport {
  type Tree = Cofree[Chain, RawTag]
  type Res = Cofree[Chain, ResolvedOp]

  def resolve(op: Tree): Res =
    Cofree
      .cata[Chain, ResolvedOp, Res](resolveOnMoves(op).value) {
        case (SeqRes, children) =>
          Eval.later(
            Cofree(
              SeqRes,
              Eval.now(children.flatMap {
                case Cofree(SeqRes, ch) => ch.value
                case cf => Chain.one(cf)
              })
            )
          )
        case (head, children) => Eval.later(Cofree(head, Eval.now(children)))
      }
      .value

  def resolveOnMoves(op: Tree): Eval[Res] =
    RawCursor(NonEmptyList.one(ChainZipper.one(op))).cata { rc =>
      OptionT[Eval, ChainZipper[Res]](
        ({
          case SeqTag => SeqRes
          case ParTag => ParRes
          case XorTag => XorRes
          case NextTag(item) => NextRes(item)
          case CallServiceTag(serviceId, funcName, call) =>
            CallServiceRes(
              serviceId,
              funcName,
              call,
              rc.currentPeerId
                .getOrElse(LiteralModel.initPeerId)
            )
        }: PartialFunction[RawTag, ResolvedOp]).lift
          .apply(rc.tag)
          .map(MakeRes.leaf)
          .traverse(c =>
            Eval.later(
              ChainZipper(
                through(rc.pathFromPrev),
                c,
                through(rc.pathToNext)
              )
            )
          )
      )

    }
      .map(NonEmptyChain.fromChain(_).map(_.uncons))
      .map {
        case None =>
          // TODO should never happen
          Cofree(SeqRes, MakeRes.nilTail)
        case Some((el, `nil`)) => el
        case Some((el, tail)) =>
          // TODO this is also very strange
          Cofree(SeqRes, Eval.now(el +: tail))
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
