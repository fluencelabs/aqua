package aqua

import cats.effect.{IO, IOApp}
import cats.data.Validated

import java.nio.file.Paths

object Test extends IOApp.Simple {

  override def run: IO[Unit] =
    AquaCompiler
      .compileFilesTo[IO](
        Paths.get("./aqua-src"),
        LazyList(Paths.get("./aqua")),
        Paths.get("./target"),
        AquaCompiler.TypescriptTarget
      )
      .map {
        case Validated.Invalid(errs) =>
          errs.map(println)
        case Validated.Valid(res) =>
          res.map(println)
      }

}
