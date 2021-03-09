package aqua.interim.names

import aqua.interim.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}

sealed trait NameOp[T]

case class ReadName[F[_]](name: Name[F]) extends NameOp[Type]
case class ReadArrow[F[_]](name: Name[F]) extends NameOp[ArrowType]
case class DefineName[F[_]](name: Name[F], `type`: Type) extends NameOp[Unit]
case class EraseName[F[_]](name: Name[F]) extends NameOp[Unit]

case class BeginScope[F[_]](token: Token[F]) extends NameOp[Unit]
case class EndScope[F[_]]() extends NameOp[Unit]
