package aqua.api

import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiled
import aqua.files.FileModuleId

import cats.data.Chain
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Test extends IOApp.Simple {

  override def run: IO[Unit] = {

    APICompilation
      .compilePath(
//        "/home/diemust/git/decider/src/aqua/decider/poll.aqua",
        "./aqua-src/antithesis.aqua",
        Imports.fromMap(
          Map(
            "/" -> Map(
              "" -> List(
                "/home/diemust/git/decider/.fluence/aqua",
                "/home/diemust/git/decider/.fluence/aqua-dependencies/node_modules",
              )
            )
          )
        ),
        AquaAPIConfig(logLevel = "info"),
        TypeScriptBackend(false, "IFluenceClient$$")
      ).timed
      .flatMap { case (duration, res) =>
        println("Compilation time: " + duration.toMillis)
        val (warnings, result) = res.value.run

        IO.delay {
          // warnings.toList.foreach(println)
        } *> result.fold(
          errors =>
            IO.delay {
              errors.toChain.toList.foreach(println)
            },
          compiled => {
            val content = compiled.get(0).get.compiled.head.content
            val targetPath = Path("./target/antithesis.ts")

            Stream
              .emit(content)
              .through(text.utf8.encode)
              .through(Files[IO].writeAll(targetPath))
              .attempt
              .compile
              .last *> IO.delay(
              println(s"File: ${targetPath.absolute.normalize}")
            )
          }
        )

      }
  }
}
