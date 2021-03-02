package aqua.model

import aqua.parser.{Block, DefAlias, DefFunc, DefService, DefType, ExecOp, FuncOp, InstrOp, On, Par}
import cats.Functor
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import shapeless._
import cats.syntax.functor._

import scala.collection.immutable.Queue

trait Passer[F[_], I <: HList, O <: HList] {
  type Out = O

  def exitFuncOpGroup(group: FuncOp[F, I], last: O): O

  def funcOpCtx(op: FuncOp[F, I], prev: O): O

  def blockCtx(block: Block[F, I]): O

  def mapFuncOp(op: FuncOp[F, I], prev: O): FuncOp[F, O] = {
    val ctx = funcOpCtx(op, prev)
    op match {
      case p @ Par(_, inner, _) =>
        val inOp = mapFuncOp(inner, ctx)
        p.copy(op = inOp.asInstanceOf[InstrOp[F, O]], context = exitFuncOpGroup(p, inOp.context))
      case o @ On(_, ops, _) =>
        val (inOps, inCtx) = mapFuncOps(ops, ctx)
        o.copy(
          ops = inOps.asInstanceOf[NonEmptyList[ExecOp[F, O]]],
          context = exitFuncOpGroup(o, exitFuncOpGroup(o, inCtx))
        )
      case _ =>
        op.as(ctx)
    }
  }

  def mapFuncOps(ops: NonEmptyList[FuncOp[F, I]], context: O): (NonEmptyList[FuncOp[F, O]], O) = {
    val (queue, lastCtx) = ops.foldLeft[(Queue[FuncOp[F, O]], O)](Queue.empty -> context) {
      case ((acc, o), op) =>
        val mapped = mapFuncOp(op, o)
        acc.appended(mapped) -> mapped.context
    }
    NonEmptyList.fromListUnsafe(queue.toList) -> lastCtx
  }

  def emptyCtx: Out

  def combineBlockCtx(prev: Out, block: Out): Out

  def mapBlock(block: Block[F, I], prevCtx: Out): (List[F[String]], Block[F, Out]) = {
    val ctx = blockCtx(block)
    val dupErr = duplicates(prevCtx, ctx)
    val bCtx = combineBlockCtx(prevCtx, ctx)
    val combinedBlock = block match {
      case df @ DefFunc(_, body, _) =>
        val (newBody, bodyCtx) = mapFuncOps(body, bCtx)
        df.copy(body = newBody, context = bodyCtx)
      case ds: DefService[F, I] =>
        ds.copy(context = bCtx)
      case al: DefAlias[F, I] =>
        al.copy(context = bCtx)
      case dt: DefType[F, I] =>
        dt.copy(context = bCtx)
    }

    (dupErr ::: unresolved(combinedBlock.context)) -> combinedBlock
  }

  def andThen[O2 <: HList](f: Passer[F, I, O] => Passer[F, I, O2]): Passer[F, I, O2] = f(this)

  def duplicates(prev: Out, next: Out): List[F[String]]
  def unresolved(ctx: Out): List[F[String]]

  def pass(blocks: List[Block[F, I]]): ValidatedNel[F[String], List[Block[F, Out]]] = {
    val (errs, _, nblocks) =
      blocks.foldLeft[(Queue[F[String]], Out, Queue[Block[F, Out]])]((Queue.empty, emptyCtx, Queue.empty)) {
        case ((errs, prevCtx, blockAcc), next) =>
          val (addErrs, mappedBlock) = mapBlock(next, prevCtx)
          (errs.appendedAll(addErrs), mappedBlock.context, blockAcc.appended(mappedBlock))
      }
    NonEmptyList
      .fromList(errs.toList)
      .fold[ValidatedNel[F[String], List[Block[F, Out]]]](Valid(nblocks.toList))(Invalid(_))
  }
}

object Passer {

  def hnil[F[_]]: Passer[F, HNil, HNil] =
    new Passer[F, HNil, HNil] {
      override def exitFuncOpGroup(group: FuncOp[F, HNil], last: HNil): HNil = HNil

      override def funcOpCtx(op: FuncOp[F, HNil], prev: HNil): HNil = HNil

      override def blockCtx(block: Block[F, HNil]): HNil = HNil

      override def duplicates(prev: Out, next: Out): List[F[String]] = Nil

      override def emptyCtx: Out = HNil

      override def combineBlockCtx(prev: Out, block: Out): Out = HNil

      override def unresolved(ctx: Out): List[F[String]] = Nil
    }
}
