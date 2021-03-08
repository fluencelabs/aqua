package aqua.interim.scope

import aqua.parser.lexer.Value

trait PeerIdOp[T]

case class OnPeerId[F[_]](id: Value[F]) extends PeerIdOp[Unit]
case class ErasePeerId[F[_]]() extends PeerIdOp[Unit]
