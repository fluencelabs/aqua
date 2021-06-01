package aqua.semantics.rules.types

import aqua.model.LambdaModel
import aqua.parser.lexer.{ArrowTypeToken, CustomTypeToken, LambdaOp, Name, Token, TypeToken}
import aqua.types.{ArrowType, Type}
import cats.data.NonEmptyMap

sealed trait TypeOp[F[_], T]

case class ResolveType[F[_]](token: TypeToken[F]) extends TypeOp[F, Option[Type]]
case class ResolveArrowDef[F[_]](arrowDef: ArrowTypeToken[F]) extends TypeOp[F, Option[ArrowType]]

case class DefineField[F[_]](name: Name[F], `type`: Type) extends TypeOp[F, Boolean]
case class PurgeFields[F[_]](token: Token[F]) extends TypeOp[F, Option[NonEmptyMap[String, Type]]]

case class DefineDataType[F[_]](name: CustomTypeToken[F], fields: NonEmptyMap[String, Type])
    extends TypeOp[F, Boolean]
case class DefineAlias[F[_]](name: CustomTypeToken[F], target: Type) extends TypeOp[F, Boolean]

case class ResolveLambda[F[_]](root: Type, ops: List[LambdaOp[F]])
    extends TypeOp[F, List[LambdaModel]]

case class EnsureTypeMatches[F[_]](token: Token[F], expected: Type, given: Type)
    extends TypeOp[F, Boolean]

case class CheckArgumentsNum[F[_]](token: Token[F], expected: Int, given: Int)
    extends TypeOp[F, Boolean]
