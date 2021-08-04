package aqua.semantics.rules

import aqua.model._
import aqua.parser.lexer._
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, LiteralType, Type}
import cats.data.Chain
import cats.free.Free
import cats.syntax.apply._

class ValuesAlgebra[F[_], Alg[_]](implicit N: NamesAlgebra[F, Alg], T: TypesAlgebra[F, Alg]) {

  def ensureIsString(v: Value[F]): Free[Alg, Boolean] =
    ensureTypeMatches(v, LiteralType.string)

  def ensureTypeMatches(v: Value[F], expected: Type): Free[Alg, Boolean] =
    resolveType(v).flatMap {
      case Some(vt) =>
        T.ensureTypeMatches(
          v match {
            case l: Literal[F] => l
            case VarLambda(n, lambda) => lambda.lastOption.getOrElse(n)
          },
          expected,
          vt
        )
      case None => Free.pure(false)
    }

  def resolveType(v: Value[F]): Free[Alg, Option[Type]] =
    valueToModel(v).map(_.map(_.lastType))

  def valueToModel(v: Value[F]): Free[Alg, Option[ValueModel]] =
    v match {
      case l: Literal[F] => Free.pure(Some(LiteralModel(l.value, l.ts)))
      case VarLambda(name, ops) =>
        N.read(name).flatMap {
          case Some(t) =>
            T.resolveLambda(t, ops).map(Chain.fromSeq).map(VarModel(name.value, t, _)).map(Some(_))
          case None =>
            Free.pure(None)
        }
    }

  def checkArguments(token: Token[F], arr: ArrowType, args: List[Value[F]]): Free[Alg, Boolean] =
    // TODO: do we really need to check this?
    T.checkArgumentsNumber(token, arr.domain.length, args.length).flatMap {
      case false => Free.pure[Alg, Boolean](false)
      case true =>
        args
          .map[Free[Alg, Option[(Token[F], Type)]]](tkn =>
            resolveType(tkn).map(_.map(t => tkn -> t))
          )
          .zip(arr.domain.toList)
          .foldLeft(
            Free.pure[Alg, Boolean](true)
          ) { case (f, (ft, t)) =>
            (
              f,
              ft.flatMap {
                case None =>
                  Free.pure(false)
                case Some((tkn, valType)) =>
                  T.ensureTypeMatches(tkn, t, valType)
              }
            ).mapN(_ && _)
          }
    }

}

object ValuesAlgebra {

  implicit def deriveValuesAlgebra[F[_], Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): ValuesAlgebra[F, Alg] =
    new ValuesAlgebra[F, Alg]()
}
