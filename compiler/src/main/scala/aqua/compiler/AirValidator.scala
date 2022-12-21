package aqua.compiler

import aqua.backend.AirFunction
import cats.data.ValidatedNec


trait AirValidator[F[_]] {
  def init(): F[Unit]

  def validate(
    airs: List[AirFunction]
  ): F[ValidatedNec[String, Unit]]
}
