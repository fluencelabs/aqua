package aqua

import cats.Monad
import cats.effect.IO
import cats.effect.kernel.Async
import fs2.io.file.{Files, Path}

import scala.concurrent.{ExecutionContext, Future}

object RunCommand {

  def run[F[_]: Monad: Files: AquaIO: Async](
    multiaddr: String,
    func: String,
    input: Path,
    imps: List[Path]
  )(implicit ec: ExecutionContext): F[Unit] = ???
}
