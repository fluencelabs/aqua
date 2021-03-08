package aqua.context

import aqua.context.marker.FuncArgMarker
import aqua.context.walker.Walker
import aqua.context.walker.Walker.UnresolvedError
import aqua.interim.types.{ArrayType, ArrowType, ProductType, ScalarType, Type}
import aqua.parser.lexer.{
  ArrowDef,
  DataTypeToken,
  IntoArray,
  IntoField,
  LambdaOp,
  Literal,
  TypeToken,
  Value,
  Var,
  VarLambda
}
import aqua.parser.{Block, CallExpr, Extract, FuncExpr}
import cats.{Comonad, Functor}
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.functor._
import cats.syntax.comonad._

import scala.collection.immutable.Queue

case class VarTypes[F[_]](
  vars: Map[String, Type] = Map.empty[String, Type],
  errorsQ: Queue[VarTypes.Err[F]] = Queue.empty
) {
  def error(err: VarTypes.Err[F]): VarTypes[F] = copy(errorsQ = errorsQ.appended(err))

  def errors: List[VarTypes.Err[F]] = errorsQ.toList

  def resolve(varname: String, t: Type): VarTypes[F] = copy(vars + (varname -> t))
  def get(varname: String): Option[Type] = vars.get(varname)
}

object VarTypes {
  sealed trait Err[F[_]] extends UnresolvedError[F]

  case class TypeMismatch[F[_]](point: F[Unit], expected: Type, given: Type) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Type mismatch, expected: `$expected`, given: `$given`")
  }

  case class TypeUndefined[F[_]](point: F[String]) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.map(v => s"Undefined: $v")
  }

  case class NotAnArray[F[_]](point: F[Unit], t: Type) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.as(s"Expected array, but type is $t")
  }

  case class ExpectedProduct[F[_]](point: F[String], t: Type) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.map(f => s"Expected product with field `$f`, but type is $t")
  }

  case class FieldNotFound[F[_]](point: F[String], t: ProductType) extends Err[F] {

    override def toStringF(implicit F: Functor[F]): F[String] =
      point.map(f =>
        s"Expected product with field `$f`, but type `${t.name}` has only `${t.fields.keys.toNonEmptyList.toList.mkString("`, `")}`"
      )
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
    getTypes: Selector[O, Types[F]],
    getArgsAndVars: Selector[I, ArgsAndVars[F]]
  ) extends Walker[F, I, VarTypes[F] :: O] {
    type Ctx = VarTypes[F] :: O

    override def exitFuncExprGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncExprGroup(group, last.tail)

    def getArrowDef(name: String, inCtx: I, ctx: Ctx): Option[ArrowDef[F]] =
      getArrows(inCtx).expDef.defineAcc.get(name).map(_.arrowDef)

    def getArrowType(name: String, inCtx: I, ctx: Ctx): Option[ArrowType] =
      getArrowDef(name, inCtx, ctx).flatMap(getTypes(ctx.tail).resolveArrowDef(_))

    def resolveIdent(name: Var[F], inCtx: I, prev: Ctx): Either[Err[F], Type] =
      prev.head.vars
        .get(name.name.extract)
        .orElse(
          getArgsAndVars(inCtx).expDef.defineAcc
            .get(name.name.extract)
            .collect {
              case FuncArgMarker(_, dt) =>
                getTypes(prev.tail).resolveTypeToken(dt)
            }
            .flatten
        )
        .orElse(
          getArrowType(name.name.extract, inCtx, prev)
        )
        .toRight(TypeUndefined(name.name))

    def resolveOp(rootT: Type, ops: List[LambdaOp[F]]): Either[Err[F], Type] =
      ops.headOption.fold[Either[Err[F], Type]](Right(rootT)) {
        case IntoArray(f) =>
          rootT match {
            case ArrayType(intern) => resolveOp(intern, ops.tail).map[Type](ArrayType)
            case _ => Left(NotAnArray(f, rootT))
          }
        case IntoField(name) =>
          rootT match {
            case pt @ ProductType(_, fields) =>
              fields(name.extract)
                .toRight(FieldNotFound(name, pt))
                .flatMap(resolveOp(_, ops.tail))
            case _ => Left(ExpectedProduct(name, rootT))
          }

      }

    def resolveValueType(v: Value[F], inCtx: I, prev: Ctx): Either[Err[F], Type] =
      v match {
        case Literal(_, ts) => Right(ts) // We want to collect errors with pointers!
        case VarLambda(name, lambda) =>
          resolveIdent(name, inCtx, prev).flatMap(resolveOp(_, lambda))

      }

    def funcCall(
      fc: CallExpr[F, I],
      arrowDef: ArrowDef[F],
      prev: Ctx
    ): VarTypes[F] = {
      val funcType = getTypes(prev.tail).resolveArrowDef(arrowDef)
      val (valueErrs, valueTypes) = fc.args
        .map(v => resolveValueType(v, fc.context, prev).map(_ -> v))
        .foldLeft[(Queue[Err[F]], Queue[(Type, Value[F])])](Queue.empty -> Queue.empty) {
          case ((errs, args), Right(t)) => (errs, args.appended(t))
          case ((errs, args), Left(t)) => (errs.appended(t), args)
        }

      if (valueErrs.nonEmpty) valueErrs.foldLeft(prev.head)(_.error(_))
      else {
        funcType.fold(prev.head) {
          case ft if ft.args.length != valueTypes.length =>
            prev.head.error(ArgNumMismatch(fc.arrow.unit, ft.args.length, valueTypes.length))
          case ft =>
            ft.args.zip(valueTypes).foldLeft(prev.head) {
              case (acc, (expectedType, (givenType, _))) if expectedType.acceptsValueOf(givenType) => acc
              case (acc, (expectedType, (givenType, v))) =>
                acc.error(TypeMismatch(v.unit, expectedType, givenType))

            }
        }
      }
    }

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx =
      (op match {
        case Extract(vr, fc, _) =>
          getArrowDef(fc.arrow.name.extract, fc.context, prev)
            .fold(prev.head.error(ArrowUntyped(fc.arrow.unit, fc.arrow.name.extract))) { arrowDef =>
              val withFC = funcCall(fc, arrowDef, prev)

              arrowDef.resType
                .flatMap(getTypes(prev.tail).resolveTypeToken)
                .fold(withFC)(withFC.resolve(vr.name.extract, _))
            }

        case fc: CallExpr[F, I] =>
          getArrowDef(fc.arrow.name.extract, fc.context, prev)
            .fold(prev.head.error(ArrowUntyped(fc.arrow.unit, fc.arrow.name.extract))) { arrowDef =>
              funcCall(fc, arrowDef, prev)
            }

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
