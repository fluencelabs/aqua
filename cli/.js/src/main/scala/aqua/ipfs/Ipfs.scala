package aqua.ipfs

import aqua.ipfs.js.IpfsApi
import aqua.js.{Fluence, PeerConfig}
import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

import scala.concurrent.{ExecutionContext, Future}

object Ipfs {

  def upload[F[_]: Async](data: Vector[Byte], multiaddr: String)(implicit
    ec: ExecutionContext
  ): F[Boolean] = {
    Async[F]
      .fromFuture(
        (for {
          _ <- Fluence
            .start(
              PeerConfig(
                multiaddr,
                10000,
                "info",
                null
              )
            )
            .toFuture
          peer = Fluence.getPeer()
          rpcApiResult <- IpfsApi.getExternalApiMultiaddr(peer.getStatus().peerId).toFuture
          rpcMultiaddr <-
            if (rpcApiResult.success) {
              Future.successful(rpcApiResult.multiaddr)
            } else {
              Future.failed(new RuntimeException(rpcApiResult.error))
            }
        } yield true).pure[F]
      )
  }
}
