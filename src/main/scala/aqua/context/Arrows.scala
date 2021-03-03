package aqua.context

import aqua.context.marker.{AbilityArrow, ArrowMarker, FuncArrow, LocalArrow}
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.{AbilityFuncCall, Block, DefFunc, DefService, Extract, FuncCall, FuncExpr}
import aqua.parser.lexer.{ArrowName, ArrowType}
import cats.{Comonad, Functor}
import shapeless._
import cats.syntax.comonad._

case class Arrows[F[_]](expDef: ExpectAndDefine[ArrowName[F], ArrowMarker[F]]) {

  def clearLocal: Arrows[F] =
    copy(expDef.collectDefinitions {
      case fa: FuncArrow[F, _] => fa
      case aa: AbilityArrow[F, _] => aa
    })
  def clearExpectations: Arrows[F] = copy(expDef.clearExpectations)

  def expect(a: ArrowName[F])(implicit F: Comonad[F]): Arrows[F] =
    copy(expDef.combineSeq(Arrows.emptyAcc.expect(Acc.one(a.name.extract, a))))

  def defined(name: String, marker: ArrowMarker[F]): Arrows[F] =
    copy(expDef.defined(name, marker))
}

object Arrows {
  type Acc[F[_]] = ExpectAndDefine[ArrowName[F], ArrowMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, ArrowName[F], ArrowMarker[F]]
  def empty[F[_]]: Arrows[F] = Arrows[F](emptyAcc[F])

  case class DuplicateArrow[F[_]](name: String, marker: ArrowMarker[F]) extends Walker.DupError[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      marker.toError(s"Duplicate function or arrow definition: ${name}")
  }

  case class UnresolvedArrow[F[_]](name: String, usage: ArrowName[F]) extends Walker.UnresolvedError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = usage.as(s"Unresolved arrow $name")
  }

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O]) extends Walker[F, I, Arrows[F] :: O] {
    type Ctx = Arrows[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      (op match {
        case FuncCall(a, _, _) =>
          prev.head.expect(a)
        case afc: AbilityFuncCall[F, I] =>
          prev.head.expect(afc.arrow)

        case Extract(_, fc, _) =>
          fc match {
            case FuncCall(a, _, _) =>
              prev.head.expect(a)
            case afc: AbilityFuncCall[F, I] =>
              prev.head.expect(afc.arrow)
          }
        case _ =>
          prev.head
      }) :: extend.funcOpCtx(op, prev.tail)

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case DefFunc(head, _, _) =>
          head.args
            .foldLeft(empty[F]) {
              case (acc, (k, _, at: ArrowType[F])) =>
                acc.defined(k, LocalArrow(at))
              case (acc, _) => acc
            }

        case DefService(ab, funcs, _) =>
          val prefix = ab.name.extract + "."
          funcs.toNel.foldLeft(empty[F]) {
            case (acc, (n, at)) =>
              acc.defined(prefix + n, AbilityArrow(ab, at))
          }

        case _ =>
          empty[F]

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      Walker.collectDups(prev.head.expDef, next.head.expDef, DuplicateArrow[F]) ::: extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      Arrows(prev.head.expDef combineSeq block.head.expDef).clearLocal :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UnresolvedArrow[F])
      (curErrs ::: extErrs, Arrows(curExpDef) :: extCtx)
    }
  }
}
