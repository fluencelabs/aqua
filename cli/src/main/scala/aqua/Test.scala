package aqua

import aqua.compiler.AquaCompiler
import aqua.model.transform.BodyConfig
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Paths

object Test extends IOApp.Simple {

  implicit def logger[F[_]: Sync]: SelfAwareStructuredLogger[F] =
    Slf4jLogger.getLogger[F]

  override def run: IO[Unit] =
    AquaCompiler
      .compileFilesTo[IO](
        Paths.get("./aqua-src"),
        List(Paths.get("./aqua")),
        Paths.get("./target"),
        AquaCompiler.TypescriptTarget,
        BodyConfig()
      )
      .map {
        case Validated.Invalid(errs) =>
          errs.map(println)
        case Validated.Valid(_) =>

      }

}
