package aqua.air
import aqua.backend.AirString
import cats.data.ValidatedNec
import cats.effect.Async
import cats.data.Validated.validNec

import scala.concurrent.ExecutionContext

object AirValidation {

  def validate[F[_]: Async](airs: List[AirString]): F[ValidatedNec[String, Unit]] = Async[F].pure(validNec(()))

}
