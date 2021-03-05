package aqua.context

import aqua.context.walker.Walker
import aqua.context.walker.Walker.UnresolvedError
import aqua.interim.ScalarType
import aqua.parser.lexer.{ArrowDef, ArrowTypeToken, BasicTypeToken, DataTypeToken, Literal, TypeToken, VarLambda}
import aqua.parser.{Block, Extract, FuncExpr}
import cats.{Comonad, Functor}
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.functor._
import cats.syntax.comonad._

import scala.collection.immutable.Queue

case class VarTypes[F[_]](
  derived: Map[String, DataTypeToken[F]] = Map.empty[String, DataTypeToken[F]],
  errorsQ: Queue[VarTypes.Err[F]] = Queue.empty
) {
  def error(err: VarTypes.Err[F]): VarTypes[F] = copy(errorsQ = errorsQ.appended(err))

  def errors: List[VarTypes.Err[F]] = errorsQ.toList

  def derive(varname: String, dt: DataTypeToken[F]): VarTypes[F] = copy(derived + (varname -> dt))
}

object VarTypes {
  sealed trait Err[F[_]] extends UnresolvedError[F]

  case class TypeMismatch[F[_]](point: F[Unit], expected: TypeToken[F], given: TypeToken[F]) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Type mismatch, expected: `$expected`, given: `$given`")
  }

  case class LiteralTypeMismatch[F[_]: Comonad](
    point: F[Unit],
    expected: TypeToken[F],
    given: Set[ScalarType]
  ) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Literal type mismatch, expected: `${expected}`, given: (`${given.mkString("`, `")}`)")
  }

  case class VarUntyped[F[_]: Comonad](point: F[Unit], name: String) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Untyped variable: `${name}`")
  }

  case class ArrowUntyped[F[_]: Comonad](point: F[Unit], name: String) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Untyped arrow: `${name}`")
  }

  case class ArgNumMismatch[F[_]](point: F[Unit], expected: Int, given: Int) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Wrong number of arguments, expected: `$expected`, given: `$given`")
  }

  case class ArrowResultMismatch[F[_]](
    point: F[Unit],
    expected: Option[DataTypeToken[F]],
    given: Option[DataTypeToken[F]]
  ) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Arrow result mismatch, expected: `$expected`, given: `$given`")
  }

  class Checker[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O])(implicit
    getArrows: Selector[I, Arrows[F]],
    getTypes: Selector[I, Types[F]]
  ) extends Walker[F, I, VarTypes[F] :: O] {
    type Ctx = VarTypes[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    def getArrowDef(name: String, ctx: I): Option[ArrowDef[F]] =
      getArrows(ctx).expDef.defineAcc.get(name).map(_.arrowDef)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      (op match {
        case Extract(vr, c, ectx) =>
          getArrowDef(c.arrow.name.extract, ectx)
            .fold(prev.head.error(ArrowUntyped(c.arrow.unit, c.arrow.name.extract))) { arrowDef =>
              val types = getTypes(ectx)

              val varTypes =
                arrowDef.resType.fold(prev.head)(prev.head.derive(vr.name.extract, _))

              val args = arrowDef.argTypes

//              args.zip(c.args).foldLeft(checkArgsNum) {
//                case (acc, (BasicTypeToken(v), Literal(_, ts))) if ts.contains(v.extract) => acc
//                case (acc, (t, v @ Literal(_, _))) => acc.error(LiteralTypeMismatch(v.unit, t, v.ts))
//                case (acc, (t: ArrowTypeToken[F], VarLambda(name, Nil))) =>
//                  getArrowDef(name.extract, ectx).fold(
//                    acc.error(ArrowUntyped(name.void, name.extract))
//                  )(vat =>
//                  types.resolveTypeToken(t).map(_.acceptsValueOf(vat.argTypes))
//                    // TODO matcher.isArrowSubtype(t, vat)
//                    (t.resType, vat.resType) match {
//                      case (None, None) => acc
//                      case (Some(tr), Some(vr)) if isSubtype(ectx, tr, vr) => acc
//                      case _ => acc.error(ArrowResultMismatch(name.void, t.resType, vat.resType))
//                    }
//                  )
//
//                case (acc, (t, VarLambda(name, lambda))) =>
//                  // TODO find var type
//                  acc.derived
//                    .get(name.extract)
//                    .fold(
//                      // TODO undefined variable
//                      acc
//                    )(_ =>
//                      // TODO traverse lambda, find subtypes
//                      // TODO finally, check if resulting type is a subtype of expected type
//                      acc
//                    )
//              }
              ???
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
      VarTypes[F](errorsQ = prev.head.errorsQ appendedAll block.head.errorsQ) :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def duplicates(prev: Out, next: Out): List[Walker.DupError[F]] =
      extend.duplicates(prev.tail, next.tail)

    override def unresolved(ctx: Out): (List[Walker.UnresolvedError[F]], Out) = {
      val (extErr, extCtx) = extend.unresolved(ctx.tail)
      (ctx.head.errors ::: extErr, ctx.head :: extCtx)
    }
  }

}
