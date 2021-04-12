package aqua

import aqua.backend.ts.TypescriptFile
import cats.effect.{IO, IOApp}
import cats.data.Validated

import java.nio.file.Paths

object Test extends IOApp.Simple {

  override def run: IO[Unit] =
    AquaCompiler
      .prepareFiles[IO](
        Paths.get("./aqua-src"),
        LazyList(Paths.get("./aqua")),
        Paths.get("./target")
      )
      .map {
        case Validated.Invalid(errs) =>
          errs.map(println)
        case Validated.Valid(preps) =>
          preps.map(p => p.target("ts") + "\n" + TypescriptFile(p.model).generateTS()).map(println)
      }

}
