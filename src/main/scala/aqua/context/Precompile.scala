package aqua.context

import aqua.context.walker.Walker
import aqua.interim.{Func, Op}
import aqua.parser.{Block, DefFunc, FuncExpr}
import cats.Comonad
import shapeless._

case class Precompile(funcs: Map[String, Func] = Map.empty, build: Option[Op] = None)

object Precompile {

  class Pass[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O]) extends Walker[F, I, Precompile :: O] {

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Out): Out =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Out): Out = prev.head :: extend.funcOpCtx(op, prev.tail)

    override def blockCtx(block: Block[F, I]): Out =
      (block match {
        case DefFunc(head, body, context) =>
          // get func types from vartypes
          // (later) get result type
          //
          ???
      }) :: extend.blockCtx(block)

    override def emptyCtx: Out = Precompile() :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      Precompile(prev.head.funcs ++ block.head.funcs) :: extend.combineBlockCtx(prev.tail, block.tail)

    override def duplicates(prev: Out, next: Out): List[Walker.DupError[F]] = extend.duplicates(prev.tail, next.tail)

    override def unresolved(ctx: Out): (List[Walker.UnresolvedError[F]], Out) = {
      val (extErr, extOut) = extend.unresolved(ctx.tail)
      (extErr, ctx.head :: extOut)
    }
  }
}
