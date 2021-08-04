package aqua.semantics.rules.types

import aqua.model.{AquaContext, IntoArrayModel, IntoFieldModel, IntoIndexModel, LambdaModel}
import aqua.parser.lexer.{
  ArrayTypeToken,
  ArrowTypeToken,
  BasicTypeToken,
  CustomTypeToken,
  IntoArray,
  IntoField,
  IntoIndex,
  LambdaOp,
  Name,
  OptionTypeToken,
  StreamTypeToken,
  Token,
  TopBottomToken,
  TypeToken
}
import aqua.types.{
  ArrayType,
  ArrowType,
  BottomType,
  DataType,
  OptionType,
  StreamType,
  StructType,
  TopType,
  Type
}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid

case class TypesState[F[_]](
  fields: Map[String, (Name[F], Type)] = Map.empty[String, (Name[F], Type)],
  strict: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, CustomTypeToken[F]] = Map.empty[String, CustomTypeToken[F]]
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def resolveTypeToken(tt: TypeToken[F]): Option[Type] =
    tt match {
      case TopBottomToken(_, isTop) =>
        Option(if (isTop) TopType else BottomType)
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case it: DataType =>
          ArrayType(it)
        }
      case StreamTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case it: DataType =>
          StreamType(it)
        }
      case OptionTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case it: DataType =>
          OptionType(it)
        }
      case ctt: CustomTypeToken[F] => strict.get(ctt.value)
      case btt: BasicTypeToken[F] => Some(btt.value)
      case ArrowTypeToken(_, args, res) =>
        val strictArgs = args.map(resolveTypeToken).collect { case Some(dt: DataType) =>
          dt
        }
        val strictRes = res.flatMap(resolveTypeToken).collect { case dt: DataType =>
          dt
        }
        Option.when(strictRes.isDefined == res.isDefined && strictArgs.length == args.length)(
          ArrowType(strictArgs, strictRes)
        )
    }

  def resolveArrowDef(ad: ArrowTypeToken[F]): ValidatedNec[(Token[F], String), ArrowType] =
    ad.resType.flatMap(resolveTypeToken) match {
      case resType if resType.isDefined == ad.resType.isDefined =>
        val (errs, argTypes) = ad.argTypes
          .map(tt => resolveTypeToken(tt).toRight(tt -> s"Type unresolved"))
          .foldLeft[(Chain[(Token[F], String)], Chain[Type])]((Chain.empty, Chain.empty)) {
            case ((errs, argTypes), Right(at)) => (errs, argTypes.append(at))
            case ((errs, argTypes), Left(e)) => (errs.append(e), argTypes)
          }

        NonEmptyChain
          .fromChain(errs)
          .fold[ValidatedNec[(Token[F], String), ArrowType]](
            Valid(ArrowType(argTypes.toList, resType))
          )(Invalid(_))

      case _ =>
        Invalid(NonEmptyChain.one(ad.resType.getOrElse(ad) -> "Cannot resolve the result type"))
    }

  def resolveOps(
    rootT: Type,
    ops: List[LambdaOp[F]]
  ): Either[(Token[F], String), List[LambdaModel]] =
    ops match {
      case Nil => Right(Nil)
      case (i @ IntoArray(_)) :: tail =>
        rootT match {
          case ArrayType(intern) =>
            resolveOps(intern, tail).map(IntoArrayModel(ArrayType(intern)) :: _)
          case StreamType(intern) =>
            resolveOps(intern, tail).map(IntoArrayModel(ArrayType(intern)) :: _)
          case _ => Left(i -> s"Expected $rootT to be an array or a stream")
        }
      case (i @ IntoField(_)) :: tail =>
        rootT match {
          case pt @ StructType(_, fields) =>
            fields(i.value)
              .toRight(i -> s"Field `${i.value}` not found in type `${pt.name}``")
              .flatMap(t => resolveOps(t, tail).map(IntoFieldModel(i.value, t) :: _))
          case _ => Left(i -> s"Expected product to resolve a field, got $rootT")
        }
      case (i @ IntoIndex(_)) :: tail =>
        rootT match {
          case ArrayType(intern) =>
            resolveOps(intern, tail).map(IntoIndexModel(i.value, intern) :: _)
          case StreamType(intern) =>
            resolveOps(intern, tail).map(IntoIndexModel(i.value, intern) :: _)
          case OptionType(intern) =>
            i.value match {
              case 0 =>
                resolveOps(intern, tail).map(IntoIndexModel(i.value, intern) :: _)
              case _ =>
                Left(i -> s"Option might have only one element, use ! to get it")
            }

          case _ => Left(i -> s"Expected $rootT to be an array or a stream")
        }
    }
}

object TypesState {

  implicit def typesStateMonoid[F[_]]: Monoid[TypesState[F]] = new Monoid[TypesState[F]] {
    override def empty: TypesState[F] = TypesState()

    override def combine(x: TypesState[F], y: TypesState[F]): TypesState[F] =
      TypesState(
        strict = x.strict ++ y.strict,
        definitions = x.definitions ++ y.definitions
      )
  }

  def init[F[_]](context: AquaContext): TypesState[F] = TypesState(strict = context.allTypes())
}
