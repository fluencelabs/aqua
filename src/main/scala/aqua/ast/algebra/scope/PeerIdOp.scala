package aqua.ast.algebra.scope

import aqua.parser.lexer.Value

sealed trait PeerIdOp[F[_], T]

case class OnPeerId[F[_]](id: Value[F]) extends PeerIdOp[F, Unit]
case class ErasePeerId[F[_]]() extends PeerIdOp[F, Unit]
case class CurrentPeerId[F[_]]() extends PeerIdOp[F, Option[Value[F]]]
