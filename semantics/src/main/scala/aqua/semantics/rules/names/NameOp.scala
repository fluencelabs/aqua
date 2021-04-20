package aqua.semantics.rules.names

import aqua.model.{LiteralModel, ValueModel}
import aqua.parser.lexer.{Literal, Name, Token}
import aqua.types.{ArrowType, Type}

sealed trait NameOp[F[_], T]

case class ReadName[F[_]](name: Name[F]) extends NameOp[F, Option[Type]]
case class ConstantDefined[F[_]](name: Name[F]) extends NameOp[F, Option[Literal[F]]]

case class ReadArrow[F[_]](name: Name[F]) extends NameOp[F, Option[ArrowType]]

case class DefineName[F[_]](name: Name[F], `type`: Type) extends NameOp[F, Boolean]
case class DefineConstant[F[_]](name: Name[F], literal: Literal[F]) extends NameOp[F, Boolean]

case class DefineArrow[F[_]](name: Name[F], gen: ArrowType, isRoot: Boolean)
    extends NameOp[F, Boolean]

case class BeginScope[F[_]](token: Token[F]) extends NameOp[F, Unit]

case class EndScope[F[_]]() extends NameOp[F, Unit]
