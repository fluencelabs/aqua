package aqua.context

import aqua.context.walker.Walker
import aqua.context.walker.Walker.UnresolvedError
import aqua.parser.lexer.{AquaArrowType, ArrowType, DataType, Type}
import aqua.parser.{Block, Extract, FuncExpr}
import cats.{Comonad, Functor}
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.functor._
import cats.syntax.comonad._

case class VarTypes[F[_]](
  derived: Map[String, DataType[F]] = Map.empty[String, DataType[F]],
  errors: List[VarTypes.TypeMismatch[F]] = Nil
)

object VarTypes {

  case class TypeMismatch[F[_]](point: F[Unit], expected: Type[F], given: Type[F]) extends UnresolvedError[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Type mismatch, expected: `$expected`, given: `$given`")
  }

  class Checker[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O])(implicit
    getArrows: Selector[I, Arrows[F]]
  ) extends Walker[F, I, VarTypes[F] :: O] {
    type Ctx = VarTypes[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      (op match {
        case Extract(vr, c, ectx) =>
          getArrows(ectx).expDef.defineAcc.get(c.arrow.name.extract).map(_.arrowDef).map {
            case at: ArrowType[F] =>
              println(s"Type of $vr is ${at.res}")
              if (at.args.length != c.args.length)
                println(s"Error: expected ${at.args.length} arguments, got ${c.args.length}")
              else
                at.args.zip(c.args).map {
                  case (expectedType, value) =>
                    // TODO: check literal
                    // TODO check VarLambda
                    println(s"Expected $expectedType, got $value")
                }
            case ad: AquaArrowType[F] =>
              println("Error: cannot set variable from a function with no return type")
              if (ad.args.length != c.args.length)
                println(s"Error: expected ${ad.args.length} arguments, got ${c.args.length}")
              else
                ad.args.zip(c.args).map {
                  case (expectedType, value) =>
                    // TODO: check literal
                    // TODO check VarLambda
                    // TODO check LocalArrow as well
                    println(s"Expected $expectedType, got $value")
                }
              ad
          }

          prev.head

        case _ =>
          prev.head
      }) :: extend.funcOpCtx(op, prev.tail)

    override def blockCtx(block: Block[F, I]): Ctx =
      VarTypes[F]() :: extend.blockCtx(block)

    override def emptyCtx: Ctx =
      VarTypes[F]() :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      VarTypes[F](errors = prev.head.errors ::: block.head.errors) :: extend.combineBlockCtx(prev.tail, block.tail)

    override def duplicates(prev: Out, next: Out): List[Walker.DupError[F]] =
      extend.duplicates(prev.tail, next.tail)

    override def unresolved(ctx: Out): (List[Walker.UnresolvedError[F]], Out) = {
      val (extErr, extCtx) = extend.unresolved(ctx.tail)
      (ctx.head.errors ::: extErr, ctx.head :: extCtx)
    }
  }

}
