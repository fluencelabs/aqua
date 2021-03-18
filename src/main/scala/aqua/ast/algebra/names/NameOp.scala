package aqua.ast.algebra.names

import aqua.ast.algebra.types.Type
import aqua.ast.gen.ArrowGen
import aqua.parser.lexer.{Name, Token}

sealed trait NameOp[F[_], T]

case class ReadName[F[_]](name: Name[F]) extends NameOp[F, Option[Type]]

case class ReadArrow[F[_]](name: Name[F]) extends NameOp[F, Option[ArrowGen]]

case class DefineName[F[_]](name: Name[F], `type`: Type) extends NameOp[F, Boolean]

case class DefineArrow[F[_]](name: Name[F], gen: ArrowGen, isRoot: Boolean) extends NameOp[F, Boolean]

case class BeginScope[F[_]](token: Token[F]) extends NameOp[F, Unit]

case class EndScope[F[_]]() extends NameOp[F, Unit]
