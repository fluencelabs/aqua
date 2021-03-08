package aqua.interim.scope

import aqua.parser.lexer.Value
import cats.InjectK
import cats.free.Free

class PeerIdAlgebra[Alg[_]](implicit I: InjectK[PeerIdOp, Alg]) {

  def onPeerId[F[_]](id: Value[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](OnPeerId[F](id))

  def erasePeerId(): Free[Alg, Unit] =
    Free.liftInject[Alg](ErasePeerId())

}
