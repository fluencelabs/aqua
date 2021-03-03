package aqua.context.scope

import aqua.parser.lexer.Value
import cats.Functor
import cats.syntax.functor._

case class Scope[F[_]](mode: Option[F[Mode]] = None, peerId: Option[Value[F]] = None) {
  def par(f: F[Unit])(implicit F: Functor[F]): Scope[F] = copy(mode = Some(f.as(ParMode)))

  def xor(f: F[Unit])(implicit F: Functor[F]): Scope[F] = copy(mode = Some(f.as(XorMode)))

  def on(v: Value[F]): Scope[F] = copy(peerId = Some(v))

  def unsetMode: Scope[F] = copy(mode = None)

  def unsetPeer: Scope[F] = copy(peerId = None)
}
