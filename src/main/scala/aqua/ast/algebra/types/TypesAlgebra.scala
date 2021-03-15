package aqua.ast.algebra.types

import aqua.parser.lexer.{ArrowDef, CustomTypeToken, LambdaOp, Name, Token, TypeToken}
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.free.Free

class TypesAlgebra[F[_], Alg[_]](implicit T: InjectK[TypeOp[F, *], Alg]) {

  def resolveType(token: TypeToken[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ResolveType(token))

  def resolveArrowDef(arrowDef: ArrowDef[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](ResolveArrowDef(arrowDef))

  def defineField(name: Name[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineField[F](name, `type`))

  def purgeFields(): Free[Alg, NonEmptyList[(Name[F], Type)]] =
    Free.liftInject[Alg](PurgeFields[F]())

  def defineDataType(name: CustomTypeToken[F], fields: NonEmptyMap[String, Type]): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineDataType(name, fields))

  def defineAlias(name: CustomTypeToken[F], target: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineAlias(name, target))

  def resolveLambda(root: Type, ops: List[LambdaOp[F]]): Free[Alg, Option[Type]] =
    Free.liftInject[Alg](ResolveLambda(root, ops))

  def ensureTypeMatches(token: Token[F], expected: Type, given: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](EnsureTypeMatches[F](token, expected, given))

}

object TypesAlgebra {

  implicit def typesAlgebra[F[_], Alg[_]](implicit T: InjectK[TypeOp[F, *], Alg]): TypesAlgebra[F, Alg] =
    new TypesAlgebra[F, Alg]()
}
