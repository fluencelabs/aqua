package aqua.semantics.rules.types

import aqua.semantics.rules.ReportError
import aqua.parser.lexer.Token

import aqua.types.{ArrowType, ProductType}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyMap, State}
import cats.~>
import monocle.Lens
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.collection.immutable.SortedMap

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
              .foldLeft[S[Option[ArrowType]]](State.pure(None)) { case (n, (tkn, hint)) =>
                report(tkn, hint) >> n
              }
        }

      case df: DefineField[F] =>
        getState.map(_.fields.get(df.name.value)).flatMap {
          case None =>
            modify(st => st.copy(fields = st.fields.updated(df.name.value, df.name -> df.`type`)))
              .as(true)
          case Some(_) =>
            report(df.name, s"Cannot define field `${df.name.value}`, it was already defined above")
              .as(false)
        }
      case pf: PurgeFields[F] =>
        getState
          .map(_.fields.view.mapValues(_._2))
          .map(SortedMap.from(_))
          .map(NonEmptyMap.fromMap(_))
          .flatMap {
            case Some(fs) => modify(_.copy(fields = Map.empty)).as(Some(fs))
            case None => report(pf.token, "Cannot define a data type without fields").as(None)
          }

      case ddt: DefineDataType[F] =>
        getState.map(_.definitions.get(ddt.name.value)).flatMap {
          case Some(n) if n == ddt.name => State.pure(false)
          case Some(_) =>
            report(ddt.name, s"Type `${ddt.name.value}` was already defined").as(false)
          case None =>
            modify(st =>
              st.copy(
                strict = st.strict.updated(ddt.name.value, ProductType(ddt.name.value, ddt.fields)),
                definitions = st.definitions.updated(ddt.name.value, ddt.name)
              )
            )
              .as(true)
        }

      case da: DefineAlias[F] =>
        getState.map(_.definitions.get(da.name.value)).flatMap {
          case Some(n) if n == da.name => State.pure(false)
          case Some(_) => report(da.name, s"Type `${da.name.value}` was already defined").as(false)
          case None =>
            modify(st =>
              st.copy(
                strict = st.strict.updated(da.name.value, da.target),
                definitions = st.definitions.updated(da.name.value, da.name)
              )
            ).as(true)
        }

      case rl: ResolveLambda[F] =>
        getState.map(_.resolveOps(rl.root, rl.ops)).flatMap {
          case Left((tkn, hint)) => report(tkn, hint).as(None)
          case Right(t) => State.pure(Some(t))
        }

      case etm: EnsureTypeMatches[F] =>
        // TODO in case of two literals, check for types intersection?
        if (etm.expected.acceptsValueOf(etm.`given`)) State.pure(true)
        else
          report(etm.token, s"Types mismatch, expected: ${etm.expected}, given: ${etm.`given`}")
            .as(false)

      case ca: CheckArgumentsNum[F] =>
        if (ca.expected == ca.given) State.pure(true)
        else
          report(
            ca.token,
            s"Number of arguments doesn't match the function type, expected: ${ca.expected}, given: ${ca.`given`}"
          ).as(false)
    }
}
