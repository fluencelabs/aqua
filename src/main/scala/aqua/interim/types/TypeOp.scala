package aqua.interim.types

import aqua.parser.lexer.{ArrowDef, CustomTypeToken, TypeToken, Var}
import cats.data.NonEmptyMap

sealed trait TypeOp[T]

case class ResolveType[F[_]](token: TypeToken[F]) extends TypeOp[Type]
case class ResolveArrowDef[F[_]](arrowDef: ArrowDef[F]) extends TypeOp[ArrowType]

case class DefineField[F[_]](name: Var[F], `type`: Type) extends TypeOp[Unit]
case class DefineDataType[F[_]](name: CustomTypeToken[F], fields: NonEmptyMap[String, Type]) extends TypeOp[Unit]
case class DefineAlias[F[_]](name: CustomTypeToken[F], target: Type) extends TypeOp[Unit]
