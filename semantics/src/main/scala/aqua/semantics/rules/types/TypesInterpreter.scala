package aqua.semantics.rules.types

import aqua.model.LambdaModel
import aqua.parser.lexer.{ArrowTypeToken, CustomTypeToken, LambdaOp, Name, Token, TypeToken}
import aqua.semantics.rules.ReportError
import aqua.types.{ArrowType, StructType, Type}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyMap, State}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.~>
import monocle.Lens

import scala.collection.immutable.SortedMap

class TypesInterpreter[F[_], X](implicit lens: Lens[X, TypesState[F]], error: ReportError[F, X])
    extends TypesAlgebra[F, State[X, *]] {

  type ST[A] = State[X, A]

  protected def getState: ST[TypesState[F]] = State.get.map(lens.get)
  protected def setState(st: TypesState[F]): ST[Unit] = State.modify(s => lens.replace(st)(s))

  protected def report(t: Token[F], hint: String): ST[Unit] =
    State.modify(error(_, t, hint))

  protected def modify(f: TypesState[F] => TypesState[F]): ST[Unit] =
    State.modify(lens.modify(f))

  override def resolveType(token: TypeToken[F]): State[X, Option[Type]] =
    getState.map(_.resolveTypeToken(token)).flatMap {
      case Some(t) => State.pure(Some(t))
      case None => report(token, s"Unresolved type").as(None)
    }

  override def resolveArrowDef(arrowDef: ArrowTypeToken[F]): State[X, Option[ArrowType]] =
    getState.map(_.resolveArrowDef(arrowDef)).flatMap {
      case Valid(t) => State.pure[X, Option[ArrowType]](Some(t))
      case Invalid(errs) =>
        errs
          .foldLeft[ST[Option[ArrowType]]](State.pure(None)) { case (n, (tkn, hint)) =>
            report(tkn, hint) >> n
          }
    }

  override def defineField(name: Name[F], `type`: Type): State[X, Boolean] =
    getState.map(_.fields.get(name.value)).flatMap {
      case None =>
        modify(st => st.copy(fields = st.fields.updated(name.value, name -> `type`)))
          .as(true)
      case Some(_) =>
        report(name, s"Cannot define field `${name.value}`, it was already defined above")
          .as(false)
    }

  override def purgeFields(token: Token[F]): State[X, Option[NonEmptyMap[String, Type]]] =
    getState
      .map(_.fields.view.mapValues(_._2))
      .map(SortedMap.from(_))
      .map(NonEmptyMap.fromMap(_))
      .flatMap {
        case Some(fs) => modify(_.copy(fields = Map.empty)).as(Some(fs))
        case None => report(token, "Cannot define a data type without fields").as(None)
      }

  override def defineDataType(
    name: CustomTypeToken[F],
    fields: NonEmptyMap[String, Type]
  ): State[X, Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case Some(n) if n == name => State.pure(false)
      case Some(_) =>
        report(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify(st =>
          st.copy(
            strict = st.strict.updated(name.value, StructType(name.value, fields)),
            definitions = st.definitions.updated(name.value, name)
          )
        )
          .as(true)
    }

  override def defineAlias(name: CustomTypeToken[F], target: Type): State[X, Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case Some(n) if n == name => State.pure(false)
      case Some(_) => report(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify(st =>
          st.copy(
            strict = st.strict.updated(name.value, target),
            definitions = st.definitions.updated(name.value, name)
          )
        ).as(true)
    }

  override def resolveLambda(root: Type, ops: List[LambdaOp[F]]): State[X, List[LambdaModel]] =
    getState.map(_.resolveOps(root, ops)).flatMap {
      case Left((tkn, hint)) => report(tkn, hint).as(Nil)
      case Right(ts) => State.pure(ts)
    }

  override def ensureTypeMatches(
    token: Token[F],
    expected: Type,
    givenType: Type
  ): State[X, Boolean] =
    // TODO in case of two literals, check for types intersection?
    if (expected.acceptsValueOf(givenType)) State.pure(true)
    else
      report(token, s"Types mismatch, expected: ${expected}, given: ${givenType}")
        .as(false)

  override def expectNoExport(token: Token[F]): State[X, Unit] =
    report(
      token,
      "Types mismatch. Cannot assign to a variable the result of a call that returns nothing"
    ).as(())

  override def checkArgumentsNumber(
    token: Token[F],
    expected: Int,
    givenNum: Int
  ): State[X, Boolean] =
    if (expected == givenNum) State.pure(true)
    else
      report(
        token,
        s"Number of arguments doesn't match the function type, expected: ${expected}, given: ${givenNum}"
      ).as(false)

}
