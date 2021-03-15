package aqua.ast.algebra.names

import aqua.ast.algebra.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}

sealed trait NameOp[F[_], T]

case class ReadName[F[_]](name: Name[F]) extends NameOp[F, Option[Type]]

case class ReadArrow[F[_]](name: Name[F]) extends NameOp[F, Option[ArrowType]]

case class DefineName[F[_]](name: Name[F], `type`: Type, isRoot: Boolean) extends NameOp[F, Boolean]

case class BeginScope[F[_]](token: Token[F]) extends NameOp[F, Unit]

case class EndScope[F[_]]() extends NameOp[F, Unit]
