package aqua.air
import aqua.backend.AirFunction
import cats.data.ValidatedNec
import cats.effect.Async
import cats.data.Validated.validNec

import scala.concurrent.ExecutionContext

object AirValidation {

  def init[F[_]: Async](): F[Unit] = Async[F].pure(())

  def validate[F[_]: Async](airs: List[AirFunction]): F[ValidatedNec[String, Unit]] = Async[F].pure(validNec(()))

}
