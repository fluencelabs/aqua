package aqua.api

import aqua.backend.air.AirBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiled
import aqua.files.FileModuleId

import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Test extends IOApp.Simple {

  override def run: IO[Unit] = {
    APICompilation
      .compilePath(
        "/home/vagrant/decider/src/aqua/decider/poll.aqua",
        Imports.fromMap(
          Map(
            "/home/vagrant/decider" -> Map(
              "" -> List("/home/vagrant/decider/.fluence/aqua"),
              "@fluencelabs/aqua-ipfs" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-ipfs"
              ),
              "@fluencelabs/installation-spell" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/installation-spell"
              ),
              "@fluencelabs/spell" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/spell"
              ),
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              ),
              "@fluencelabs/registry" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/registry"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-ipfs" -> Map(
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/installation-spell" -> Map(
              "@fluencelabs/aqua-ipfs" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-ipfs"
              ),
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              ),
              "@fluencelabs/spell" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/installation-spell/node_modules/@fluencelabs/spell"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/spell" -> Map(
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/registry" -> Map(
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              ),
              "@fluencelabs/trust-graph" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/trust-graph"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/installation-spell/node_modules/@fluencelabs/spell" -> Map(
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              )
            ),
            "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/trust-graph" -> Map(
              "@fluencelabs/aqua-lib" -> List(
                "/home/vagrant/decider/.fluence/aqua-dependencies/node_modules/@fluencelabs/aqua-lib"
              )
            )
          )
        ),
        AquaAPIConfig(targetType = TargetType.AirType),
        AirBackend
      )
      .timed
      .flatMap { case (time, res) =>
        val (warnings, result) = res.value.run

        result.fold(
          errors =>
            IO.delay {
              errors.toChain.toList.foreach(println)
            },
          compiled => {
            val content = compiled.get(0).get.compiled.head.content
            IO.delay {
              println(s"Whole compilation took ${time.toMillis} ms")
            }
            // val targetPath = Path("./target/antithesis.ts")

            // Stream
            //   .emit(content)
            //   .through(text.utf8.encode)
            //   .through(Files[IO].writeAll(targetPath))
            //   .attempt
            //   .compile
            //   .last *> IO.delay(
            //   println(s"File: ${targetPath.absolute.normalize}")
            // )
          }
        )

      }
  }
}
