package aqua.semantics.rules.types

import aqua.model.LambdaModel
import aqua.parser.lexer._
import aqua.types.{ArrowType, Type}
import cats.InjectK
import cats.data.NonEmptyMap
import cats.free.Free

class TypesAlgebra[F[_], Alg[_]](implicit T: InjectK[TypeOp[F, *], Alg]) {

  def resolveType(token: TypeToken[F]): Free[Alg, Option[Type]] =
    Free.liftInject[Alg](ResolveType(token))

  def resolveArrowDef(arrowDef: ArrowTypeToken[F]): Free[Alg, Option[ArrowType]] =
    Free.liftInject[Alg](ResolveArrowDef(arrowDef))

  def defineField(name: Name[F], `type`: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineField[F](name, `type`))

  def purgeFields(token: Token[F]): Free[Alg, Option[NonEmptyMap[String, Type]]] =
    Free.liftInject[Alg](PurgeFields[F](token))

  def defineDataType(
    name: CustomTypeToken[F],
    fields: NonEmptyMap[String, Type]
  ): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineDataType(name, fields))

  def defineAlias(name: CustomTypeToken[F], target: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineAlias(name, target))

  def resolveLambda(root: Type, ops: List[LambdaOp[F]]): Free[Alg, List[LambdaModel]] =
    Free.liftInject[Alg](ResolveLambda(root, ops))

  def expectStream(token: Token[F], stream: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](ExpectStream[F](token, stream))

  def expectStreamMember(token: Token[F], stream: Type, member: Option[Type]): Free[Alg, Unit] =
    Free.liftInject[Alg](ExpectStreamMember[F](token, stream, member))

  def ensureTypeMatches(token: Token[F], expected: Type, given: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](EnsureTypeMatches[F](token, expected, given))

  def expectNoExport(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](ExpectNoExport[F](token))

  def checkArgumentsNumber(token: Token[F], expected: Int, given: Int): Free[Alg, Boolean] =
    Free.liftInject[Alg](CheckArgumentsNum(token, expected, given))
}

object TypesAlgebra {

  implicit def typesAlgebra[F[_], Alg[_]](implicit
    T: InjectK[TypeOp[F, *], Alg]
  ): TypesAlgebra[F, Alg] =
    new TypesAlgebra[F, Alg]()
}
