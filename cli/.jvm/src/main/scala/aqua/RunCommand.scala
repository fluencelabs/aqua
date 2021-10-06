package aqua

import aqua.parser.expr.CallArrowExpr
import cats.Monad
import cats.effect.IO
import fs2.io.file.{Files, Path}
import cats.~>
import cats.Id

import scala.concurrent.{ExecutionContext, Future}

object RunCommand {
  def run[F[_]: Monad: Files: AquaIO](multiaddr: String, func: String, input: Path, imps: List[Path])(implicit F: Future ~> F, ec: ExecutionContext): F[Unit] = ???
}
