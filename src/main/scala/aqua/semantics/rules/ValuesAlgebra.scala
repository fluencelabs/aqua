package aqua.semantics.rules

import aqua.generator.DataView
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.parser.lexer.{IntoArray, IntoField, LambdaOp, Literal, Token, Value, VarLambda}
import aqua.semantics.{ArrowType, LiteralType, Type}
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
    v match {
      case l: Literal[F] =>
        Free pure [Alg, Option[Type]] Some(l.ts)
      case VarLambda(n, lambda) =>
        N.read(n).flatMap {
          case Some(t) =>
            T.resolveLambda(t, lambda)
          case None =>
            Free.pure(None)
        }
    }

  def checkArguments(arr: ArrowType, args: List[Value[F]]): Free[Alg, Boolean] =
    args
      .map[Free[Alg, Option[(Token[F], Type)]]] {
        case l: Literal[F] => Free.pure(Some(l -> l.ts))
        case VarLambda(n, ops) =>
          N.read(n).flatMap {
            case Some(t) => T.resolveLambda(t, ops).map(_.map(ops.lastOption.getOrElse(n) -> _))
            case None => Free.pure(None)
          }
      }
      // TODO check that number of arguments matches!
      .zip(arr.args)
      .foldLeft(
        Free.pure[Alg, Boolean](true)
      ) { case (f, (ft, t)) =>
        (
          f,
          ft.flatMap {
            case None => Free.pure(false)
            case Some((tkn, valType)) =>
              T.ensureTypeMatches(tkn, t, valType)
          }
        ).mapN(_ && _)
      }

}

object ValuesAlgebra {

  private def opsToLens[F[_]](ops: List[LambdaOp[F]]): String =
    ops match {
      case Nil => ""
      case (_: IntoArray[F]) :: tail => "[@" + opsToLens(tail) + "]"
      case (f: IntoField[F]) :: tail => "." + f.value + opsToLens(tail)
    }

  def valueToData[F[_]](v: Value[F]): DataView =
    v match {
      case l: Literal[F] => DataView.StringScalar(l.value)
      case VarLambda(name, Nil) => DataView.Variable(name.value)
      case VarLambda(name, ops) => DataView.VarLens(name.value, opsToLens(ops))
    }

  private def argsToData[F[_]](args: List[Value[F]]): List[DataView] = args.map(valueToData)

  implicit def deriveValuesAlgebra[F[_], Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): ValuesAlgebra[F, Alg] =
    new ValuesAlgebra[F, Alg]()
}
