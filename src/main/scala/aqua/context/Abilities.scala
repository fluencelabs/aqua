package aqua.context

import aqua.context.marker.{AbilityMarker, ServiceAbility}
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.{AbilityFuncCall, AbilityId, Block, DefService, Extract, FuncExpr}
import aqua.parser.lexer.Ability
import cats.{Comonad, Functor}
import shapeless._
import cats.syntax.comonad._

case class Abilities[F[_]](expDef: ExpectAndDefine[Ability[F], AbilityMarker[F]]) {
  def clearDefinitions: Abilities[F] = copy(expDef.clearDefinitions)

  def clearExpectations: Abilities[F] = copy(expDef.clearExpectations)

  def expect(a: Ability[F])(implicit F: Comonad[F]): Abilities[F] =
    copy(expDef.expect(Acc.one(a.name.extract, a)))
}

object Abilities {
  type Acc[F[_]] = ExpectAndDefine[Ability[F], AbilityMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, Ability[F], AbilityMarker[F]]
  def empty[F[_]]: Abilities[F] = Abilities[F](emptyAcc[F])

  case class DuplicateAbility[F[_]](name: String, marker: AbilityMarker[F]) extends Walker.DupError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = marker.toError(s"Duplicate ability definition: ${name}")
  }

  case class UndefinedAbility[F[_]](name: String, usage: Ability[F]) extends Walker.UnresolvedError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = usage.as(s"Undefined ability $name")
  }

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O]) extends Walker[F, I, Abilities[F] :: O] {
    type Ctx = Abilities[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      (op match {
        case aid: AbilityId[F, I] =>
          prev.head.expect(aid.ability)
        case ac: AbilityFuncCall[F, I] =>
          prev.head.expect(ac.ability)
        case Extract(_, ac: AbilityFuncCall[F, I], _) =>
          prev.head.expect(ac.ability)
        case _ =>
          prev.head
      }) :: extend.funcOpCtx(op, prev.tail)

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case defs: DefService[F, I] =>
          Abilities(emptyAcc[F].defined(defs.name.name.extract, ServiceAbility(defs.name, defs)))
        case _ =>
          empty[F]

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      Walker.collectDups(prev.head.expDef, next.head.expDef, DuplicateAbility[F]) ::: extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      Abilities(prev.head.expDef combineSeq block.head.expDef) :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UndefinedAbility[F])
      (curErrs ::: extErrs, Abilities(curExpDef) :: extCtx)
    }
  }
}
