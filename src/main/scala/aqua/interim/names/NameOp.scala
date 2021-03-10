package aqua.interim.names

import aqua.interim.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}

sealed trait NameOp[T] {
  type CF[_]
}

object NameOp {
  type Aux[F[_], T] = NameOp[T] { type CF[A] = F[A] }
}

case class ReadName[F[_]](name: Name[F]) extends NameOp[Type] {
  type CF[A] = F[A]
}

case class ReadArrow[F[_]](name: Name[F]) extends NameOp[ArrowType] {
  type CF[A] = F[A]
}

case class DefineName[F[_]](name: Name[F], `type`: Type) extends NameOp[Unit] {
  type CF[A] = F[A]
}

case class EraseName[F[_]](name: Name[F]) extends NameOp[Unit] {
  type CF[A] = F[A]
}

case class BeginScope[F[_]](token: Token[F]) extends NameOp[Unit] {
  type CF[A] = F[A]
}

case class EndScope[F[_]]() extends NameOp[Unit] {
  type CF[A] = F[A]
}
