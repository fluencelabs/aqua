package aqua.context

import aqua.context.marker.{AbilityResolveMarker, ResolvedMarker}
import aqua.context.scope.Scope
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.lexer.Ability
import aqua.parser._
import cats.syntax.comonad._
import cats.{Comonad, Functor}
import shapeless._
import shapeless.ops.hlist.Selector

case class AbilitiesResolve[F[_]](expDef: ExpectAndDefine[Ability[F], AbilityResolveMarker[F]]) {

  def resolve[L](ar: AbilityResolve[F, L])(implicit F: Comonad[F]): AbilitiesResolve[F] =
    copy(
      expDef.copy(defineAcc = expDef.defineAcc.updated(ar.ability.name.extract, ResolvedMarker(ar)))
    )

  def clearDefinitions: AbilitiesResolve[F] = copy(expDef.clearDefinitions)

  def expect(a: Ability[F])(implicit F: Comonad[F]): AbilitiesResolve[F] =
    copy(expDef.expect(Acc.one(a.name.extract, a)))
}

object AbilitiesResolve {
  type Acc[F[_]] = ExpectAndDefine[Ability[F], AbilityResolveMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, Ability[F], AbilityResolveMarker[F]]
  def empty[F[_]]: AbilitiesResolve[F] = AbilitiesResolve[F](emptyAcc[F])

  case class UnresolvedAbility[F[_]](name: String, usage: Ability[F]) extends Walker.UnresolvedError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = usage.as(s"Unresolved ability $name")
  }

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O])(implicit getScope: Selector[O, Scope[F]])
      extends Walker[F, I, AbilitiesResolve[F] :: O] {
    type Ctx = AbilitiesResolve[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      (group match {
        case _ => last.head.clearDefinitions
      }) :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx = {
      lazy val in = extend.funcOpCtx(op, prev.tail)
      val data = prev.head
      lazy val mode = getScope(in).mode.map(_.extract)

      def combinedWith(other: AbilitiesResolve[F] => AbilitiesResolve[F]): AbilitiesResolve[F] =
        AbilitiesResolve[F](data.expDef.combine(other(empty[F]).expDef, mode))

      (op match {
        case ac: AbilityFuncCall[F, I] =>
          combinedWith(_.expect(ac.ability))
        case Extract(_, ac: AbilityFuncCall[F, I], _) =>
          combinedWith(_.expect(ac.ability))
        case ar: AbilityResolve[F, I] =>
          combinedWith(_.resolve(ar))
        case _ =>
          prev.head
      }) :: in
    }

    override def blockCtx(block: Block[F, I]): Ctx =
      empty[F] :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      AbilitiesResolve(prev.head.expDef combineSeq block.head.expDef).clearDefinitions :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UnresolvedAbility[F])
      (curErrs ::: extErrs, AbilitiesResolve(curExpDef) :: extCtx)
    }
  }
}
