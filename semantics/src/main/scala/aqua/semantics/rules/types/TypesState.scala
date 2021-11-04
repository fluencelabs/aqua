package aqua.semantics.rules.types

import aqua.model.{
  AquaContext,
  IntoArrayModel,
  IntoFieldModel,
  IntoIndexModel,
  LambdaModel,
  ValueModel
}
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
  ProductType,
  StreamType,
  StructType,
  TopType,
  Type
}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map.empty[String, (Name[S], Type)],
  strict: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, CustomTypeToken[S]] = Map.empty[String, CustomTypeToken[S]],
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def resolveTypeToken(tt: TypeToken[S]): Option[Type] =
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
      case ctt: CustomTypeToken[S] => strict.get(ctt.value)
      case btt: BasicTypeToken[S] => Some(btt.value)
      case ArrowTypeToken(_, args, res) =>
        val strictArgs = args.map(_._2).map(resolveTypeToken).collect { case Some(dt: DataType) =>
          dt
        }
        val strictRes: List[DataType] = res.flatMap(resolveTypeToken).collect { case dt: DataType =>
          dt
        }
        Option.when(strictRes.length == res.length && strictArgs.length == args.length)(
          ArrowType(ProductType(strictArgs), ProductType(strictRes.toList))
        )
    }

  def resolveArrowDef(ad: ArrowTypeToken[S]): ValidatedNec[(Token[S], String), ArrowType] = {
    val resType = ad.res.map(resolveTypeToken)

    NonEmptyChain
      .fromChain(Chain.fromSeq(ad.res.zip(resType).collect { case (dt, None) =>
        dt -> "Cannot resolve the result type"
      }))
      .fold[ValidatedNec[(Token[S], String), ArrowType]] {
        val (errs, argTypes) = ad.args.map { (argName, tt) =>
          resolveTypeToken(tt)
            .toRight(tt -> s"Type unresolved")
            .map(argName.map(_.value) -> _)
        }
          .foldLeft[(Chain[(Token[S], String)], Chain[(Option[String], Type)])](
            (Chain.empty, Chain.empty)
          ) {
            case ((errs, argTypes), Right(at)) => (errs, argTypes.append(at))
            case ((errs, argTypes), Left(e)) => (errs.append(e), argTypes)
          }

        NonEmptyChain
          .fromChain(errs)
          .fold[ValidatedNec[(Token[S], String), ArrowType]](
            Valid(
              ArrowType(
                ProductType.maybeLabelled(argTypes.toList),
                ProductType(resType.flatten.toList)
              )
            )
          )(Invalid(_))
      }(Invalid(_))
  }

  def resolveOps(
    rootT: Type,
    ops: List[LambdaOp[S]]
  ): Either[(Token[S], String), List[LambdaModel]] =
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

  case class Frame[S[_]](
    token: ArrowTypeToken[S],
    arrowType: ArrowType,
    retVals: Option[List[ValueModel]]
  )

  implicit def typesStateMonoid[S[_]]: Monoid[TypesState[S]] = new Monoid[TypesState[S]] {
    override def empty: TypesState[S] = TypesState()

    override def combine(x: TypesState[S], y: TypesState[S]): TypesState[S] =
      TypesState(
        strict = x.strict ++ y.strict,
        definitions = x.definitions ++ y.definitions
      )
  }

  def init[S[_]](context: AquaContext): TypesState[S] =
    TypesState(strict = context.allTypes())
}
