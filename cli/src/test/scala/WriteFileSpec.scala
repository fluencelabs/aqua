import aqua.AquaCompiler
import aqua.model.transform.BodyConfig
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

class WriteFileSpec extends AnyFlatSpec with Matchers {
  "cli" should "compile aqua code in js" in {
    val src = Paths.get("./cli/src/test/aqua")
    val targetTs = Paths.get("./cli/src/test/compiled/ts")
    val targetJs = Paths.get("./cli/src/test/compiled/js")
    val targetAir = Paths.get("./cli/src/test/compiled/air")

    val bc = BodyConfig()
    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetTs, AquaCompiler.TypescriptTarget, bc)
      .unsafeRunSync()
    val targetTsFile = targetTs.resolve("test.ts")
    targetTsFile.toFile.exists() should be(true)
    Files.deleteIfExists(targetTsFile)

    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetJs, AquaCompiler.JavaScriptTarget, bc)
      .unsafeRunSync()
    val targetJsFile = targetJs.resolve("test.js")
    targetJsFile.toFile.exists() should be(true)
    Files.deleteIfExists(targetJsFile)

    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetAir, AquaCompiler.AirTarget, bc)
      .unsafeRunSync()
    val targetAirFileFirst = targetAir.resolve("test.first.air")
    val targetAirFileSecond = targetAir.resolve("test.second.air")
    val targetAirFileThird = targetAir.resolve("test.third.air")
    targetAirFileFirst.toFile.exists() should be(true)
    targetAirFileSecond.toFile.exists() should be(true)
    targetAirFileThird.toFile.exists() should be(true)

    Seq(targetAirFileFirst, targetAirFileSecond, targetAirFileThird).map(Files.deleteIfExists)
  }

}
