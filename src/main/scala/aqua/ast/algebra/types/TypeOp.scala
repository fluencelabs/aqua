package aqua.ast.algebra.types

import aqua.parser.lexer.{ArrowDef, CustomTypeToken, LambdaOp, Name, Token, TypeToken}
import cats.data.{NonEmptyList, NonEmptyMap}

sealed trait TypeOp[F[_], T]

case class ResolveType[F[_]](token: TypeToken[F]) extends TypeOp[F, Type]
case class ResolveArrowDef[F[_]](arrowDef: ArrowDef[F]) extends TypeOp[F, ArrowType]

case class DefineField[F[_]](name: Name[F], `type`: Type) extends TypeOp[F, Unit]
case class PurgeFields[F[_]]() extends TypeOp[F, NonEmptyList[(Name[F], Type)]]
case class DefineDataType[F[_]](name: CustomTypeToken[F], fields: NonEmptyMap[String, Type]) extends TypeOp[F, Unit]
case class DefineAlias[F[_]](name: CustomTypeToken[F], target: Type) extends TypeOp[F, Unit]

case class ResolveLambda[F[_]](root: Type, ops: List[LambdaOp[F]]) extends TypeOp[F, Type]
case class EnsureTypeMatches[F[_]](token: Token[F], expected: Type, given: Type) extends TypeOp[F, Unit]
