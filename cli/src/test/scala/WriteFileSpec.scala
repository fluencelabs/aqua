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
    if (!Files.exists(targetTs)) Files.createDirectory(targetTs)
    val targetJs = Paths.get("./cli/src/test/compiled/js")
    if (!Files.exists(targetJs)) Files.createDirectory(targetJs)
    val targetAir = Paths.get("./cli/src/test/compiled/air")
    if (!Files.exists(targetAir)) Files.createDirectory(targetAir)

    val bc = BodyConfig()
    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetTs, AquaCompiler.TypescriptTarget, bc)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetTsFile = targetTs.resolve("test.ts")
    targetTsFile.toFile.exists() should be(true)
    Files.deleteIfExists(targetTsFile)

    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetJs, AquaCompiler.JavaScriptTarget, bc)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetJsFile = targetJs.resolve("test.js")
    targetJsFile.toFile.exists() should be(true)
    Files.deleteIfExists(targetJsFile)

    AquaCompiler
      .compileFilesTo[IO](src, LazyList.empty, targetAir, AquaCompiler.AirTarget, bc)
      .unsafeRunSync()
      .leftMap { err =>
        println(err)
        err
      }
      .isValid should be(true)
    val targetAirFileFirst = targetAir.resolve("test.first.air")
    val targetAirFileSecond = targetAir.resolve("test.second.air")
    val targetAirFileThird = targetAir.resolve("test.third.air")
    targetAirFileFirst.toFile.exists() should be(true)
    targetAirFileSecond.toFile.exists() should be(true)
    targetAirFileThird.toFile.exists() should be(true)

    Seq(targetAirFileFirst, targetAirFileSecond, targetAirFileThird).map(Files.deleteIfExists)
  }

}
