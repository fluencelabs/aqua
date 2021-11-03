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

class TypesInterpreter[S[_], X](implicit lens: Lens[X, TypesState[S]], error: ReportError[S, X])
    extends TypesAlgebra[S, State[X, *]] {

  type ST[A] = State[X, A]

  protected def getState: ST[TypesState[S]] = State.get.map(lens.get)
  protected def setState(st: TypesState[S]): ST[Unit] = State.modify(s => lens.replace(st)(s))

  protected def report(t: Token[S], hint: String): ST[Unit] =
    State.modify(error(_, t, hint))

  protected def modify(f: TypesState[S] => TypesState[S]): ST[Unit] =
    State.modify(lens.modify(f))

  override def resolveType(token: TypeToken[S]): State[X, Option[Type]] =
    getState.map(_.resolveTypeToken(token)).flatMap {
      case Some(t) => State.pure(Some(t))
      case None => report(token, s"Unresolved type").as(None)
    }

  override def resolveArrowDef(arrowDef: ArrowTypeToken[S]): State[X, Option[ArrowType]] =
    getState.map(_.resolveArrowDef(arrowDef)).flatMap {
      case Valid(t) => State.pure[X, Option[ArrowType]](Some(t))
      case Invalid(errs) =>
        errs
          .foldLeft[ST[Option[ArrowType]]](State.pure(None)) { case (n, (tkn, hint)) =>
            report(tkn, hint) >> n
          }
    }

  override def defineField(name: Name[S], `type`: Type): State[X, Boolean] =
    getState.map(_.fields.get(name.value)).flatMap {
      case None =>
        modify(st => st.copy(fields = st.fields.updated(name.value, name -> `type`)))
          .as(true)
      case Some(_) =>
        report(name, s"Cannot define field `${name.value}`, it was already defined above")
          .as(false)
    }

  override def purgeFields(token: Token[S]): State[X, Option[NonEmptyMap[String, Type]]] =
    getState
      .map(_.fields.view.mapValues(_._2))
      .map(SortedMap.from(_))
      .map(NonEmptyMap.fromMap(_))
      .flatMap {
        case Some(fs) => modify(_.copy(fields = Map.empty)).as(Some(fs))
        case None => report(token, "Cannot define a data type without fields").as(None)
      }

  override def defineDataType(
    name: CustomTypeToken[S],
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

  override def defineAlias(name: CustomTypeToken[S], target: Type): State[X, Boolean] =
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

  override def resolveLambda(root: Type, ops: List[LambdaOp[S]]): State[X, List[LambdaModel]] =
    getState.map(_.resolveOps(root, ops)).flatMap {
      case Left((tkn, hint)) => report(tkn, hint).as(Nil)
      case Right(ts) => State.pure(ts)
    }

  override def ensureTypeMatches(
    token: Token[S],
    expected: Type,
    givenType: Type
  ): State[X, Boolean] =
    // TODO in case of two literals, check for types intersection?
    if (expected.acceptsValueOf(givenType)) State.pure(true)
    else
      report(token, s"Types mismatch, expected: ${expected}, given: ${givenType}")
        .as(false)

  override def expectNoExport(token: Token[S]): State[X, Unit] =
    report(
      token,
      "Types mismatch. Cannot assign to a variable the result of a call that returns nothing"
    ).as(())

  override def checkArgumentsNumber(
    token: Token[S],
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
