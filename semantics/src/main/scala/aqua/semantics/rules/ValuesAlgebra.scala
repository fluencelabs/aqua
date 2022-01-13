package aqua.semantics.rules

import aqua.parser.lexer.*
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, LiteralType, Type}
import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.instances.list.*

class ValuesAlgebra[S[_], Alg[_]: Monad](implicit
  N: NamesAlgebra[S, Alg],
  T: TypesAlgebra[S, Alg]
) {

  def ensureIsString(v: Value[S]): Alg[Boolean] =
    ensureTypeMatches(v, LiteralType.string)

  def ensureTypeMatches(v: Value[S], expected: Type): Alg[Boolean] =
    resolveType(v).flatMap {
      case Some(vt) =>
        T.ensureTypeMatches(
          v match {
            case l: Literal[S] => l
            case VarLambda(n, lambda) => lambda.lastOption.getOrElse(n)
          },
          expected,
          vt
        )
      case None => false.pure[Alg]
    }

  def resolveType(v: Value[S]): Alg[Option[Type]] =
    valueToRaw(v).map(_.map(_.lastType))

  def valueToRaw(v: Value[S]): Alg[Option[ValueRaw]] =
    v match {
      case l: Literal[S] => Some(LiteralRaw(l.value, l.ts)).pure[Alg]
      case VarLambda(name, ops) =>
        N.read(name).flatMap {
          case Some(t) =>
            ops.traverse {
              case op: IntoField[S] =>
                T.resolveField(t, op)
              case op: IntoIndex[S] =>
                op.idx
                  .fold[Alg[Option[ValueRaw]]](Option(LiteralRaw.Zero).pure[Alg])(valueToRaw)
                  .flatMap {
                    case None => None.pure[Alg]
                    case Some(vv) => T.resolveIndex(t, op, vv)
                  }
            }.map(_.flatten).map {
              case lambda if lambda.length == ops.length =>
                Some(VarRaw(name.value, t, Chain.fromSeq(lambda)))
              case _ => None
            }

          case None =>
            None.pure[Alg]
        }
    }

  def checkArguments(token: Token[S], arr: ArrowType, args: List[Value[S]]): Alg[Boolean] =
    // TODO: do we really need to check this?
    T.checkArgumentsNumber(token, arr.domain.length, args.length).flatMap {
      case false => false.pure[Alg]
      case true =>
        args
          .map[Alg[Option[(Token[S], Type)]]](tkn => resolveType(tkn).map(_.map(t => tkn -> t)))
          .zip(arr.domain.toList)
          .foldLeft(
            true.pure[Alg]
          ) { case (f, (ft, t)) =>
            (
              f,
              ft.flatMap {
                case None =>
                  false.pure[Alg]
                case Some((tkn, valType)) =>
                  T.ensureTypeMatches(tkn, t, valType)
              }
            ).mapN(_ && _)
          }
    }

}

object ValuesAlgebra {

  implicit def deriveValuesAlgebra[S[_], Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): ValuesAlgebra[S, Alg] =
    new ValuesAlgebra[S, Alg]
}
