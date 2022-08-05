package aqua.air

import aqua.backend.AirString
import aqua.js.Fluence
import cats.data.Validated.{invalid, validNec}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.effect.Async
import cats.syntax.traverse.*
import cats.syntax.functor.*

import scala.concurrent.ExecutionContext
import scala.scalajs.js

object AirValidation {

  def validate[F[_]: Async](
    airs: List[AirString]
  ): F[ValidatedNec[String, Unit]] =
    Async[F].fromFuture {

      Async[F].executionContext.map { implicit ec =>
        for {
          _ <- Fluence.start(js.undefined).toFuture
          statuses <- airs
            .map(a => Fluence.getPeer().internals.parseAst(a.air).toFuture.map(s => (a.name, s)))
            .sequence
        } yield {
          val errors = NonEmptyChain.fromSeq(statuses.filterNot(_._2.success))
          errors.map { errs =>
            val errorsStrs = errs.map { case (fName, status) =>
              s"Cannot compile AIR for '$fName' function: ${js.JSON.stringify(status.data)}\n\n" +
              "This is unexpected error. Please, dump your Aqua code and make an issue here https://github.com/fluencelabs/aqua/issues."
            }
            invalid(errorsStrs)
          }.getOrElse(validNec(()))
        }
      }
    }

}
