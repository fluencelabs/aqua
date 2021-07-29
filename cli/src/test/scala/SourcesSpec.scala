import aqua.AquaIO
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class SourcesSpec extends AnyFlatSpec with Matchers {
  implicit val aquaIO: AquaIO[IO] = AquaFilesIO.summon[IO]

  "AquaFileSources" should "generate correct fileId with imports" in {
    val path = Paths.get("cli/src/test/test-dir")
    val importPath = path.resolve("imports")

    val sourceGen = new AquaFileSources[IO](path, List(importPath))

    val result = sourceGen.sources.unsafeRunSync()
    result.isValid shouldBe true

    val (id, importFile) = result.getOrElse(Chain.empty).headOption.get
    id.file.toString.split("/").last shouldBe "index.aqua"
    importFile.nonEmpty shouldBe true
  }

  "AquaFileSources" should "throw an error if a source file doesn't exist" in {
    val path = Paths.get("some/random/path")

    val sourceGen = new AquaFileSources[IO](path, List())

    val result = sourceGen.sources.unsafeRunSync()
    result.isInvalid shouldBe true
  }

  "AquaFileSources" should "throw an error if there is no import that is indicated in a source" in {
    val path = Paths.get("cli/src/test/test-dir")
    val importPath = path.resolve("random/import/path")

    val sourceGen = new AquaFileSources[IO](path, List(importPath))
    val result =
      sourceGen.resolve(FileModuleId(path.resolve("no-file.aqua")), "no/file").unsafeRunSync()
    result.isInvalid shouldBe true
  }
}
