package aqua

import aqua.parser.expr.CallArrowExpr
import cats.Monad
import cats.effect.IO
import fs2.io.file.Files
import cats.~>
import cats.Id

import scala.concurrent.Future

object RunCommand {
  def run[F[_]: Monad: Files: AquaIO](func: CallArrowExpr[Id])(implicit F: Future ~> F): F[Unit] = ???
}
