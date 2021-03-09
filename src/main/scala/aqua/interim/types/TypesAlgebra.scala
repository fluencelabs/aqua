package aqua.interim.types

import aqua.parser.lexer.{ArrowDef, CustomTypeToken, LambdaOp, Token, TypeToken, Var}
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.free.Free

class TypesAlgebra[Alg[_]](implicit T: InjectK[TypeOp, Alg]) {

  def resolveType[F[_]](token: TypeToken[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ResolveType(token))

  def resolveArrowDef[F[_]](arrowDef: ArrowDef[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](ResolveArrowDef(arrowDef))

  def defineField[F[_]](name: Var[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineField(name, `type`))

  def purgeFields[F[_]](): Free[Alg, NonEmptyList[(Var[F], Type)]] =
    Free.liftInject[Alg](PurgeFields[F]())

  def defineDataType[F[_]](name: CustomTypeToken[F], fields: NonEmptyMap[String, Type]): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineDataType(name, fields))

  def defineAlias[F[_]](name: CustomTypeToken[F], target: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineAlias(name, target))

  def resolveLambda[F[_]](root: Type, ops: List[LambdaOp[F]]): Free[Alg, Type] =
    Free.liftInject[Alg](ResolveLambda(root, ops))

  def ensureTypeMatches[F[_]](token: Token[F], expected: Type, given: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](EnsureTypeMatches[F](token, expected, given))

}
