package aqua

import cats.effect.kernel.Async
import fs2.io.file.Path
import cats.syntax.applicative.*

object PlatformPackagePath {
  def getPackagePath[F[_]: Async](path: String): F[Path] = Path("").pure[F]
  def getGlobalNodeModulePath: List[Path] = Nil
}
