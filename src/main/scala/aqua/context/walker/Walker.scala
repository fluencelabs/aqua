package aqua.context.walker

import aqua.context.marker.Marker
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.parser._
import aqua.parser.lexer.Token
import cats.Functor
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.functor._
import shapeless._

import scala.collection.immutable.Queue

trait Walker[F[_], I <: HList, O <: HList] {
  type Out = O

  def exitFuncExprGroup(group: FuncExpr[F, I], last: O): O

  def funcOpCtx(op: FuncExpr[F, I], prev: O): O

  def blockCtx(block: Block[F, I]): O

  def mapFuncOp(op: FuncExpr[F, I], prev: O): FuncExpr[F, O] = {
    val ctx = funcOpCtx(op, prev)
    op match {
      case p @ Par(_, inner, _) =>
        val inOp = mapFuncOp(inner, ctx)
        p.copy(op = inOp.asInstanceOf[InstrExpr[F, O]], context = exitFuncExprGroup(p, inOp.context))
      case o @ On(_, ops, _) =>
        val (inOps, inCtx) = mapFuncOps(ops, ctx)
        o.copy(
          ops = inOps.asInstanceOf[NonEmptyList[ExecExpr[F, O]]],
          context = exitFuncExprGroup(o, exitFuncExprGroup(o, inCtx))
        )
      case _ =>
        op.as(ctx)
    }
  }

  def mapFuncOps(ops: NonEmptyList[FuncExpr[F, I]], context: O): (NonEmptyList[FuncExpr[F, O]], O) = {
    val (queue, lastCtx) = ops.foldLeft[(Queue[FuncExpr[F, O]], O)](Queue.empty -> context) {
      case ((acc, o), op) =>
        val mapped = mapFuncOp(op, o)
        acc.appended(mapped) -> mapped.context
    }
    NonEmptyList.fromListUnsafe(queue.toList) -> lastCtx
  }

  def emptyCtx: Out

  def combineBlockCtx(prev: Out, block: Out): Out

  def mapBlock(block: Block[F, I], prevCtx: Out): (List[Walker.Error[F]], Block[F, Out]) = {
    val ctx = blockCtx(block)

    val dupErr = duplicates(prevCtx, ctx)
    val (unresolvedErrs, combinedBlock) = block match {
      case df @ DefFunc(_, body, _) =>
        val (newBody, bodyCtx) = mapFuncOps(body, ctx)
        val (errs, bCtx) = unresolved(combineBlockCtx(prevCtx, bodyCtx))
        errs -> df.copy(body = newBody, context = bCtx)
      case ds: DefService[F, I] =>
        val (unresErrs, bCtx) = unresolved(combineBlockCtx(prevCtx, ctx))
        unresErrs -> ds.copy(context = bCtx)
      case al: DefAlias[F, I] =>
        val (unresErrs, bCtx) = unresolved(combineBlockCtx(prevCtx, ctx))
        unresErrs -> al.copy(context = bCtx)
      case dt: DefType[F, I] =>
        val (unresErrs, bCtx) = unresolved(combineBlockCtx(prevCtx, ctx))
        unresErrs -> dt.copy(context = bCtx)
    }

    (dupErr ::: unresolvedErrs) -> combinedBlock
  }

  def andThen[O2 <: HList](f: Walker[F, I, O] => Walker[F, I, O2]): Walker[F, I, O2] = f(this)

  def duplicates(prev: Out, next: Out): List[DupError[F]]
  def unresolved(ctx: Out): (List[UnresolvedError[F]], Out)

  def walkValidate(blocks: List[Block[F, I]]): ValidatedNel[Walker.Error[F], List[Block[F, Out]]] = {
    val (errs, _, nblocks) =
      blocks.foldLeft[(Queue[Walker.Error[F]], Out, Queue[Block[F, Out]])]((Queue.empty, emptyCtx, Queue.empty)) {
        case ((errs, prevCtx, blockAcc), next) =>
          val (addErrs, mappedBlock) = mapBlock(next, prevCtx)
          (errs.appendedAll(addErrs), mappedBlock.context, blockAcc.appended(mappedBlock))
      }
    NonEmptyList
      .fromList(errs.toList)
      .fold[ValidatedNel[Walker.Error[F], List[Block[F, Out]]]](Valid(nblocks.toList))(Invalid(_))
  }
}

object Walker {

  trait Error[F[_]] {
    def toStringF(implicit F: Functor[F]): F[String]
  }
  trait DupError[F[_]] extends Error[F]
  trait UnresolvedError[F[_]] extends Error[F]

  def collectDups[F[_], A <: Token[F], B <: Marker[F]](
    prev: ExpectAndDefine[A, B],
    next: ExpectAndDefine[A, B],
    toErr: (String, B) => DupError[F]
  ): List[DupError[F]] =
    next.defineAcc.view.filterKeys(prev.defineAcc.keySet).toList.map {
      case (k, v) => toErr(k, v)
    }

  def collectUnresolved[F[_], A <: Token[F], B <: Marker[F]](
    expDef: ExpectAndDefine[A, B],
    toErr: (String, A) => UnresolvedError[F]
  ): (List[UnresolvedError[F]], ExpectAndDefine[A, B]) =
    expDef.expectAcc.data.flatMap {
      case (k, vs) => vs.toList.map(toErr(k, _))
    }.toList -> expDef.clearExpectations

  def hnil[F[_]]: Walker[F, HNil, HNil] =
    new Walker[F, HNil, HNil] {
      override def exitFuncExprGroup(group: FuncExpr[F, HNil], last: HNil): HNil = HNil

      override def funcOpCtx(op: FuncExpr[F, HNil], prev: HNil): HNil = HNil

      override def blockCtx(block: Block[F, HNil]): HNil = HNil

      override def duplicates(prev: Out, next: Out): List[DupError[F]] = Nil

      override def emptyCtx: Out = HNil

      override def combineBlockCtx(prev: Out, block: Out): Out = HNil

      override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = Nil -> ctx
    }

  def noopFrom[F[_], I <: HList, O <: HList](from: Walker[F, I, O]): Walker[F, O, O] =
    new Walker[F, O, O] {
      override def exitFuncExprGroup(group: FuncExpr[F, O], last: O): O = last

      override def funcOpCtx(op: FuncExpr[F, O], prev: O): O = prev

      override def blockCtx(block: Block[F, O]): O = block.context

      override def emptyCtx: Out = from.emptyCtx

      override def combineBlockCtx(prev: Out, block: Out): Out = block

      override def duplicates(prev: Out, next: Out): List[DupError[F]] = Nil

      override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = (Nil, ctx)
    }
}
