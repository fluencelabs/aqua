package aqua

import aqua.model.LiteralModel
import aqua.model.transform.TransformConfig
import aqua.run.RunConfig
import cats.Monad
import cats.effect.IO
import cats.effect.kernel.Async
import fs2.io.file.{Files, Path}

import scala.concurrent.{ExecutionContext, Future}

object RunCommand {

  def run[F[_]: Files: AquaIO: Async](
    multiaddr: String,
    func: String,
    args: List[LiteralModel],
    input: Path,
    imports: List[Path],
    timeout: Int,
    transformConfig: TransformConfig = TransformConfig(),
    runConfig: RunConfig = RunConfig()
  )(implicit ec: ExecutionContext): F[Unit] = ???
}
