package aqua.compiler

import aqua.backend.AirString
import cats.data.ValidatedNec


trait AirValidator[F[_]] {
  def init(): F[Unit]

  def validate(
    airs: List[AirString]
  ): F[ValidatedNec[String, Unit]]
}
