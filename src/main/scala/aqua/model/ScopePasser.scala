package aqua.model

import aqua.parser.{Block, FuncOp, On, Par}
import cats.Functor
import shapeless._

class ScopePasser[F[_]: Functor, I <: HList, O <: HList](extend: Passer[F, I, O]) extends Passer[F, I, Scope[F] :: O] {
  type Ctx = Scope[F] :: O

  override def exitFuncOpGroup(group: FuncOp[F, I], last: Ctx): Ctx =
    (group match {
      case _: Par[F, I] =>
        last.head.unsetMode
      case _: On[F, I] =>
        last.head.unsetPeer
      case _ =>
        last.head
    }) :: extend.exitFuncOpGroup(group, last.tail)

  override def funcOpCtx(op: FuncOp[F, I], prev: Ctx): Ctx =
    (op match {
      case p: Par[F, I] => prev.head.par(p.f)
      case o: On[F, I] => prev.head.on(o.peer)
      case _ => prev.head
    }) :: extend.funcOpCtx(op, prev.tail)

  override def blockCtx(block: Block[F, I]): Ctx = Scope[F]() :: extend.blockCtx(block)

  override def duplicates(prev: Out, next: Out): List[F[String]] =
    extend.duplicates(prev.tail, next.tail)

  override def emptyCtx: Out = Scope[F]() :: extend.emptyCtx

  override def combineBlockCtx(prev: Out, block: Out): Out =
    Scope[F]() :: extend.combineBlockCtx(prev.tail, block.tail)

  override def unresolved(ctx: Out): List[F[String]] = Nil
}
