package aqua.context

import aqua.context.marker.{TypeAlias, TypeDef, TypeMarker}
import aqua.context.walker.Walker.{DupError, UnresolvedError}
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.interim.types.{ArrayType, ArrowType, DataType, ProductType, Type}
import aqua.parser.{Block, DefAlias, DefFunc, DefService, DefType, FuncExpr}
import aqua.parser.lexer.{ArrayTypeToken, ArrowDef, ArrowTypeToken, BasicTypeToken, CustomTypeToken, TypeToken}
import cats.data.NonEmptyMap
import cats.{Comonad, Functor}
import shapeless._
import cats.syntax.comonad._

case class Types[F[_]](
  expDef: ExpectAndDefine[CustomTypeToken[F], TypeMarker[F]],
  strict: Map[String, Type] = Map.empty
) {
  def clearDefinitions: Types[F] = copy(expDef.clearDefinitions)
  def clearExpectations: Types[F] = copy(expDef.clearExpectations)

  def resolveTypeToken(tt: TypeToken[F])(implicit F: Comonad[F]): Option[Type] =
    Types.resolveTypeToken(strict, tt)

  def resolveArrowDef(ad: ArrowDef[F])(implicit F: Comonad[F]): Option[ArrowType] =
    Types.resolveArrowDef(strict, ad)
}

object Types {

  def resolveTypeToken[F[_]: Comonad](strict: Map[String, Type], tt: TypeToken[F]): Option[Type] =
    tt match {
      case ArrayTypeToken(dtt) =>
        resolveTypeToken(strict, dtt).collect {
          case it: DataType => ArrayType(it)
        }
      case CustomTypeToken(n) => strict.get(n.extract)
      case BasicTypeToken(v) => Some(v.extract)
      case ArrowTypeToken(_, args, res) =>
        val strictArgs = args.map(resolveTypeToken(strict, _)).collect {
          case Some(dt: DataType) => dt
        }
        val strictRes = res.flatMap(resolveTypeToken(strict, _)).collect {
          case dt: DataType => dt
        }
        Option.when(strictRes.isDefined == res.isDefined && strictArgs.length == args.length)(
          ArrowType(strictArgs, strictRes)
        )
    }

  def resolveArrowDef[F[_]: Comonad](strict: Map[String, Type], ad: ArrowDef[F]): Option[ArrowType] =
    ad.resType.flatMap(resolveTypeToken(strict, _)) match {
      case resType if resType.isDefined == ad.resType.isDefined =>
        ad.argTypes.flatMap(resolveTypeToken(strict, _)) match {
          case argTypes if argTypes.length == ad.argTypes.length => Some(ArrowType(argTypes, resType))
          case _ => None
        }

      case _ => None
    }

  type Acc[F[_]] = ExpectAndDefine[CustomTypeToken[F], TypeMarker[F]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, CustomTypeToken[F], TypeMarker[F]]
  def empty[F[_]]: Types[F] = Types[F](emptyAcc[F], Map.empty)

  case class DuplicateType[F[_]](name: String, marker: TypeMarker[F]) extends Walker.DupError[F] {
    override def toStringF(implicit F: Functor[F]): F[String] = marker.toError(s"Duplicate type definition: ${name}")
  }

  case class UnresolvedType[F[_]](name: String, usage: CustomTypeToken[F]) extends Walker.UnresolvedError[F] {
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
                  .defined(deft.name.name.extract, TypeDef(deft))
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
              .defined(a.alias.name.extract, TypeAlias(a.alias, a.target))
          )

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[DupError[F]] =
      Walker.collectDups(prev.head.expDef, next.head.expDef, DuplicateType[F]) ::: extend
        .duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      Types(
        prev.head.expDef combineSeq block.head.expDef,
        block.head.expDef.defineAcc.foldLeft(prev.head.strict) {
          case (strict, (k, TypeAlias(_, marker))) =>
            resolveTypeToken(strict, marker).fold(strict)(t => strict + (k -> t))
          case (strict, (k, TypeDef(dt))) =>
            val resTypes = dt.fields.map(_._2).map(resolveTypeToken(strict, _)).toSortedMap.collect {
              case (kk, Some(t: DataType)) => kk -> t
            }
            NonEmptyMap
              .fromMap(resTypes)
              .filter(_.length == dt.fields.length)
              .fold(strict)(fields => strict + (k -> ProductType(dt.name.name.extract, fields)))

        }
      ) :: extend.combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): (List[UnresolvedError[F]], Out) = {
      val (extErrs, extCtx) = extend.unresolved(ctx.tail)
      val (curErrs, curExpDef) = Walker.collectUnresolved(ctx.head.expDef, UnresolvedType[F])
      (curErrs ::: extErrs, Types(curExpDef, ctx.head.strict) :: extCtx)
    }
  }
}
