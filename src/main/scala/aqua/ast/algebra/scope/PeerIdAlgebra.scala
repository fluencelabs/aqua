package aqua.ast.algebra.scope

import aqua.parser.lexer.Value
import cats.InjectK
import cats.free.Free

class PeerIdAlgebra[F[_], Alg[_]](implicit I: InjectK[PeerIdOp[F, *], Alg]) {

  def onPeerId(id: Value[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](OnPeerId[F](id))

  def erasePeerId(): Free[Alg, Unit] =
    Free.liftInject[Alg](ErasePeerId[F]())

  def currentPeerId(): Free[Alg, Option[Value[F]]] =
    Free.liftInject[Alg](CurrentPeerId[F]())

}

object PeerIdAlgebra {

  implicit def peerIdAlgebra[F[_], Alg[_]](implicit I: InjectK[PeerIdOp[F, *], Alg]): PeerIdAlgebra[F, Alg] =
    new PeerIdAlgebra[F, Alg]()
}
