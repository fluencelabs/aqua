package aqua.interim.names

import aqua.interim.types.Type
import aqua.parser.lexer.Var

sealed trait NameOp[T]

case class ReadName[F[_]](name: Var[F]) extends NameOp[Type]
case class DefineName[F[_]](name: Var[F], `type`: Type) extends NameOp[Unit]
case class EraseName[F[_]](name: Var[F]) extends NameOp[Unit]
