package aqua.context

import aqua.context.marker.{DataMarker, ExtractedVarMarker, FuncArgMarker}
import aqua.context.scope.Scope
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.{AbilityFuncCall, AbilityId, Block, DefFunc, Extract, FuncCall, FuncExpr, On, Par}
import aqua.parser.lexer.{DataType, Value, Var}
import cats.{Comonad, Functor}
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.comonad._

case class ArgsAndVars[F[_]](expDef: ExpectAndDefine[F, Value[F], DataMarker[F]]) {
  def clearDefinitions: ArgsAndVars[F] = copy(expDef.clearDefinitions)
  def clearExpectations: ArgsAndVars[F] = copy(expDef.clearExpectations)
}

object ArgsAndVars {
  type Acc[F[_]] = ExpectAndDefine[F, Value[F], DataMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, Value[F], DataMarker[F]]
  def empty[F[_]]: ArgsAndVars[F] = ArgsAndVars[F](emptyAcc[F])

  case class DuplicateDef[F[_]](name: String, marker: DataMarker[F]) extends Walker.DupError[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      marker.toError(s"Duplicate variable or arg definition: ${name}")
  }

  case class UnresolvedVar[F[_]](name: String, usage: Value[F]) extends Walker.UnresolvedError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = usage.as(s"Unresolved variable $name")
  }

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O])(implicit getScope: Selector[O, Scope[F]])
      extends Walker[F, I, ArgsAndVars[F] :: O] {
    type Ctx = ArgsAndVars[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx = {
      lazy val in = extend.funcOpCtx(op, prev.tail)
      val data = prev.head
      lazy val mode = getScope(in).mode.map(_.extract)
      def combinedWith(other: Acc[F] => ExpectAndDefine[F, Value[F], DataMarker[F]]): ArgsAndVars[F] =
        ArgsAndVars[F](data.expDef.combine(other(emptyAcc[F]), mode))

      op match {
        case FuncCall(_, args, _) =>
          combinedWith(_ expect Acc.fromValues(args)) :: in
        case AbilityFuncCall(_, _, args, _) =>
          combinedWith(_ expect Acc.fromValues(args)) :: in
        case ex @ Extract(n, fc, _) =>
          val f = funcOpCtx(fc, prev)
          f.head
            .copy(
              f.head.expDef.combine(empty[F].expDef defined Acc.one(n.name.extract, ExtractedVarMarker(n, ex)), mode)
            ) :: f.tail
        case AbilityId(_, id, _) =>
          combinedWith(_ expect Acc.fromValues(id :: Nil)) :: in
        case On(p, _, _) =>
          combinedWith(_ expect Acc.fromValues(p :: Nil)) :: in
        case Par(_, _, _) =>
          data :: in
      }
    }

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case DefFunc(head, _, _) =>
          head.args
            .foldLeft(empty[F]) {
              case (acc, (k, v, dt: DataType[F])) =>
                acc.copy(acc.expDef.defined(Acc.one(k, FuncArgMarker(Var(v), dt))))
              case (acc, _) => acc
            }

        case _ =>
          empty[F]

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      Walker.collectDups(prev.head.expDef, next.head.expDef, DuplicateDef[F]) ::: extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      ArgsAndVars(prev.head.expDef combineSeq block.head.expDef).clearDefinitions :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UnresolvedVar[F])
      (curErrs ::: extErrs, ArgsAndVars(curExpDef) :: extCtx)
    }
  }
}
