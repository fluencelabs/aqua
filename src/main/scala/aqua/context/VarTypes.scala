package aqua.context

import aqua.context.walker.Walker
import aqua.parser.{Block, Extract, FuncExpr}
import shapeless._
import shapeless.ops.hlist.Selector

object VarTypes {

  class Checker[F[_], I <: HList, O <: HList](extend: Walker[F, I, O])(implicit getArrows: Selector[I, Arrows[F]])
      extends Walker[F, I, O] {
    override def exitFuncExprGroup(group: FuncExpr[F, I], last: O): O = extend.exitFuncExprGroup(group, last)

    override def funcOpCtx(op: FuncExpr[F, I], prev: O): O =
//      op match {
//      case Extract(vr, c, ectx) =>
//        val arrows = getArrows(ectx)
//        val arrType = c match {
//          case ab: AbilityC
//        }
//
//        extend.funcOpCtx(op, prev)
//
//      case _ =>
      extend.funcOpCtx(op, prev)
    //   }

    override def blockCtx(block: Block[F, I]): O = extend.blockCtx(block)

    override def emptyCtx: Out = extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out = extend.combineBlockCtx(prev, block)

    override def duplicates(prev: Out, next: Out): List[Walker.DupError[F]] = extend.duplicates(prev, next)

    override def unresolved(ctx: Out): (List[Walker.UnresolvedError[F]], Out) = extend.unresolved(ctx)
  }

}
