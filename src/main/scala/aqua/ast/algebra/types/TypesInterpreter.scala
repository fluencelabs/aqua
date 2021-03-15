package aqua.ast.algebra.types

import aqua.ast.algebra.ReportError
import aqua.context.VarTypes.{Err, ExpectedProduct, FieldNotFound, NotAnArray}
import aqua.parser.lexer.{
  ArrayTypeToken,
  ArrowDef,
  ArrowTypeToken,
  BasicTypeToken,
  CustomTypeToken,
  IntoArray,
  IntoField,
  LambdaOp,
  Name,
  Token,
  TypeToken
}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptyMap, State, Validated, ValidatedNel}
import cats.free.Free
import cats.{~>, Comonad}
import monocle.Lens
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.collection.immutable.{Queue, SortedMap}

class TypesInterpreter[F[_], X](implicit lens: Lens[X, TypesState[F]], error: ReportError[F, X])
    extends (TypeOp[F, *] ~> State[X, *]) {

  type S[A] = State[X, A]
  type St = TypesState[F]

  protected def getState: S[St] = State.get.map(lens.get)
  protected def setState(st: St): S[Unit] = State.modify(s => lens.replace(st)(s))

  protected def report(t: Token[F], hint: String): S[Unit] =
    State.modify(error(_, t, hint))

  protected def modify(f: St => St): S[Unit] =
    State.modify(lens.modify(f))

  override def apply[A](fa: TypeOp[F, A]): State[X, A] =
    fa match {
      case rt: ResolveType[F] =>
        getState.map(_.resolveTypeToken(rt.token)).flatMap {
          case Some(t) => State.pure(Some(t))
          case None => report(rt.token, s"Unresolved type").as(None)
        }
      case ra: ResolveArrowDef[F] =>
        getState.map(_.resolveArrowDef(ra.arrowDef)).flatMap {
          case Valid(t) => State.pure[X, Option[ArrowType]](Some(t))
          case Invalid(errs) =>
            errs
              .foldLeft[S[Option[ArrowType]]](State.pure(None)) {
                case (n, (tkn, hint)) => report(tkn, hint) >> n
              }
        }

      case df: DefineField[F] =>
        getState.map(_.fields.get(df.name.value)).flatMap {
          case None =>
            modify(st => st.copy(fields = st.fields.updated(df.name.value, df.name -> df.`type`))).as(true)
          case Some(_) =>
            report(df.name, s"Cannot define field `${df.name.value}`, it was already defined above").as(false)
        }
      case pf: PurgeFields[F] =>
        getState.map(_.fields.view.mapValues(_._2)).map(SortedMap.from(_)).map(NonEmptyMap.fromMap(_)).flatMap {
          case Some(fs) => modify(_.copy(fields = Map.empty)).as(Some(fs))
          case None => report(pf.token, "Cannot define a data type without fields").as(None)
        }

      case ddt: DefineDataType[F] =>
        getState.map(_.isDefined(ddt.name.value)).flatMap {
          case true => report(ddt.name, s"Type `${ddt.name.value}` was already defined").as(false)
          case false =>
            modify(st => st.copy(strict = st.strict.updated(ddt.name.value, ProductType(ddt.name.value, ddt.fields))))
              .as(true)
        }

      case da: DefineAlias[F] =>
        getState.map(_.isDefined(da.name.value)).flatMap {
          case true => report(da.name, s"Type `${da.name.value}` was already defined").as(false)
          case false => modify(st => st.copy(strict = st.strict.updated(da.name.value, da.target))).as(true)
        }

      case rl: ResolveLambda[F] =>
        getState.map(_.resolveOps(rl.root, rl.ops)).flatMap {
          case Left((tkn, hint)) => report(tkn, hint).as(None)
          case Right(t) => State.pure(Some(t))
        }

      case etm: EnsureTypeMatches[F] =>
        if (etm.expected.acceptsValueOf(etm.`given`)) State.pure(true)
        else report(etm.token, s"Types mismatch, expected: ${etm.expected}, given: ${etm.`given`}").as(false)
    }
}

case class TypesState[F[_]](
  fields: Map[String, (Name[F], Type)] = Map.empty[String, (Name[F], Type)],
  strict: Map[String, Type] = Map.empty[String, Type]
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def resolveTypeToken(tt: TypeToken[F]): Option[Type] =
    tt match {
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect {
          case it: DataType => ArrayType(it)
        }
      case ctt: CustomTypeToken[F] => strict.get(ctt.value)
      case btt: BasicTypeToken[F] => Some(btt.value)
      case ArrowTypeToken(_, args, res) =>
        val strictArgs = args.map(resolveTypeToken).collect {
          case Some(dt: DataType) => dt
        }
        val strictRes = res.flatMap(resolveTypeToken).collect {
          case dt: DataType => dt
        }
        Option.when(strictRes.isDefined == res.isDefined && strictArgs.length == args.length)(
          ArrowType(strictArgs, strictRes)
        )
    }

  def resolveArrowDef(ad: ArrowTypeToken[F]): ValidatedNel[(Token[F], String), ArrowType] =
    ad.resType.flatMap(resolveTypeToken) match {
      case resType if resType.isDefined == ad.resType.isDefined =>
        val (errs, argTypes) = ad.argTypes
          .map(tt => resolveTypeToken(tt).toRight(tt -> s"Type unresolved"))
          .foldLeft[(Queue[(Token[F], String)], Queue[Type])]((Queue.empty, Queue.empty)) {
            case ((errs, argTypes), Right(at)) => (errs, argTypes.enqueue(at))
            case ((errs, argTypes), Left(e)) => (errs.enqueue(e), argTypes)
          }

        NonEmptyList
          .fromList(errs.toList)
          .fold[ValidatedNel[(Token[F], String), ArrowType]](Valid(ArrowType(argTypes.toList, resType)))(Invalid(_))

      case _ => Invalid(NonEmptyList.of(ad.resType.getOrElse(ad) -> "Cannot resolve the result type"))
    }

  def resolveOps(rootT: Type, ops: List[LambdaOp[F]]): Either[(Token[F], String), Type] =
    ops.headOption.fold[Either[(Token[F], String), Type]](Right(rootT)) {
      case i @ IntoArray(f) =>
        rootT match {
          case ArrayType(intern) => resolveOps(intern, ops.tail).map[Type](ArrayType)
          case _ => Left(i -> s"Expected $rootT to be an array")
        }
      case i @ IntoField(name) =>
        rootT match {
          case pt @ ProductType(_, fields) =>
            fields(i.value)
              .toRight(i -> s"Field `${i.value}` not found in type `${pt.name}``")
              .flatMap(resolveOps(_, ops.tail))
          case _ => Left(i -> s"Expected product to resolve a field, got $rootT")
        }

    }
}
