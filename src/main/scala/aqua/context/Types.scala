package aqua.context

import aqua.context.marker.{TypeAlias, TypeDef, TypeMarker}
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.{Block, DefAlias, DefFunc, DefService, DefType, FuncExpr}
import aqua.parser.lexer.CustomType
import cats.{Comonad, Functor}
import shapeless._
import cats.syntax.comonad._

case class Types[F[_]](expDef: ExpectAndDefine[F, CustomType[F], TypeMarker[F]]) {
  def clearDefinitions: Types[F] = copy(expDef.clearDefinitions)
  def clearExpectations: Types[F] = copy(expDef.clearExpectations)

}

object Types {
  type Acc[F[_]] = ExpectAndDefine[F, CustomType[F], TypeMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, CustomType[F], TypeMarker[F]]
  def empty[F[_]]: Types[F] = Types[F](emptyAcc[F])

  case class DuplicateType[F[_]](name: String, marker: TypeMarker[F]) extends Walker.DupError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = marker.toError(s"Duplicate type definition: ${name}")
  }

  case class UnresolvedType[F[_]](name: String, usage: CustomType[F]) extends Walker.UnresolvedError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = usage.as(s"Unresolved type $name")
  }

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O]) extends Walker[F, I, Types[F] :: O] {
    type Ctx = Types[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      prev.head :: extend.funcOpCtx(op, prev.tail)

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case DefFunc(head, _, _) =>
          head.args
            .foldLeft(empty[F]) {
              case (acc, (_, _, ft)) =>
                acc.copy(acc.expDef.expect(Acc.fromType(ft)))
            }

        case deft: DefType[F, I] =>
          Types(
            deft.fields.toNel
              .map(_._2._2)
              .map(Acc.fromType[F](_))
              .foldLeft(
                emptyAcc[F]
                  .defined(Acc.one[F, TypeMarker[F]](deft.name.name.extract, TypeDef(deft)))
              )(_ expect _)
          )

        case defs: DefService[F, I] =>
          Types(
            defs.funcs.toNel
              .map(_._2)
              .map(Acc.fromType[F](_))
              .foldLeft(
                emptyAcc[F]
              )(_ expect _)
          )
        case a: DefAlias[F, I] =>
          Types(
            emptyAcc[F]
              .expect(Acc.fromType(a.target))
              .defined(Acc.one(a.alias.name.extract, TypeAlias(a.alias, a.target)))
          )

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      Walker.collectDups(prev.head.expDef, next.head.expDef, DuplicateType[F]) ::: extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      Types(prev.head.expDef combineSeq block.head.expDef) :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UnresolvedType[F])
      (curErrs ::: extErrs, Types(curExpDef) :: extCtx)
    }
  }
}
