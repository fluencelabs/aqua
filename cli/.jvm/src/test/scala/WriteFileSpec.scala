import aqua.AquaPathCompiler
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.model.transform.TransformConfig
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fs2.io.file.{Files, Path}

class WriteFileSpec extends AnyFlatSpec with Matchers {

  "cli" should "compile aqua code in js" in {
    val src = Path("./cli/.jvm/src/test/aqua")
    val targetTs = Files[IO].createTempDirectory.unsafeRunSync()
    val targetJs = Files[IO].createTempDirectory.unsafeRunSync()
    val targetAir = Files[IO].createTempDirectory.unsafeRunSync()

    import aqua.files.AquaFilesIO.summon

    val bc = TransformConfig()
    AquaPathCompiler
      .compileFilesTo[IO](src, List.empty, Option(targetTs), TypeScriptBackend, bc, false)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetTsFile = targetTs.resolve("test.ts")
    Files[IO].exists(targetTsFile).unsafeRunSync() should be(true)
    Files[IO].deleteIfExists(targetTsFile).unsafeRunSync()

    AquaPathCompiler
      .compileFilesTo[IO](src, List.empty, Option(targetJs), JavaScriptBackend(false), bc, false)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetJsFile = targetJs.resolve("test.js")
    Files[IO].exists(targetJsFile).unsafeRunSync() should be(true)
    Files[IO].deleteIfExists(targetJsFile).unsafeRunSync()

    AquaPathCompiler
      .compileFilesTo[IO](src, List.empty, Option(targetAir), AirBackend, bc, false)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetAirFileFirst = targetAir.resolve("test.first.air")
    val targetAirFileSecond = targetAir.resolve("test.second.air")
    val targetAirFileThird = targetAir.resolve("test.third.air")
    Files[IO].exists(targetAirFileFirst).unsafeRunSync() should be(true)
    Files[IO].exists(targetAirFileSecond).unsafeRunSync() should be(true)
    Files[IO].exists(targetAirFileThird).unsafeRunSync() should be(true)

    Seq(targetAirFileFirst, targetAirFileSecond, targetAirFileThird).map(f =>
      Files[IO].deleteIfExists(f).unsafeRunSync()
    )
  }

}
