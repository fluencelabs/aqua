import aqua.AquaIO
import aqua.backend.Compiled
import aqua.compiler.AquaCompiled
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.Files
import fs2.text
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
      sourceGen.resolveImport(FileModuleId(path.resolve("no-file.aqua")), "no/file").unsafeRunSync()
    result.isInvalid shouldBe true
  }

  "AquaFileSources" should "find correct imports" in {
    val srcPath = Paths.get("cli/src/test/test-dir/index.aqua")
    val importPath = srcPath.resolve("imports")

    val sourceGen = new AquaFileSources[IO](srcPath, List(importPath))

    // should be found in importPath
    val result =
      sourceGen
        .resolveImport(FileModuleId(srcPath), "imports/import.aqua")
        .unsafeRunSync()

    result.isValid shouldBe true
    result.getOrElse(FileModuleId(Paths.get("/some/random"))).file.toFile.exists() shouldBe true

    // should be found near src file
    val result2 =
      sourceGen
        .resolveImport(FileModuleId(srcPath), "importNear.aqua")
        .unsafeRunSync()

    result2.isValid shouldBe true
    result2.getOrElse(FileModuleId(Paths.get("/some/random"))).file.toFile.exists() shouldBe true

    // near src file but in another directory
    val sourceGen2 = new AquaFileSources[IO](srcPath, List())
    val result3 =
      sourceGen2
        .resolveImport(FileModuleId(srcPath), "imports/import.aqua")
        .unsafeRunSync()

    result3.isValid shouldBe true
    result3.getOrElse(FileModuleId(Paths.get("/some/random"))).file.toFile.exists() shouldBe true
  }

  "AquaFileSources" should "resolve correct path for target" in {
    val path = Paths.get("cli/src/test/test-dir")
    val filePath = path.resolve("some-dir/file.aqua")

    val targetPath = Paths.get("/target/dir/")

    val sourceGen = new AquaFileSources[IO](path, List())

    val suffix = "_custom.super"

    val resolved = sourceGen.resolveTargetPath(filePath, targetPath, suffix)
    resolved.isValid shouldBe true

    val targetFilePath = resolved.toOption.get
    targetFilePath.toString shouldBe "/target/dir/some-dir/file_custom.super"
  }

  "AquaFileSources" should "write correct file with correct path" in {
    val path = Paths.get("cli/src/test/test-dir")
    val filePath = path.resolve("imports/import.aqua")

    val targetPath = path.resolve("target/")

    // clean up
    val resultPath = Paths.get("cli/src/test/test-dir/target/imports/import_hey.custom")
    Files[IO].deleteIfExists(resultPath).unsafeRunSync()

    val sourceGen = new AquaFileSources[IO](path, List())
    val content = "some random content"
    val compiled = AquaCompiled[FileModuleId](
      FileModuleId(filePath),
      Seq(Compiled("_hey.custom", content))
    )

    val resolved = sourceGen.write(targetPath)(compiled).unsafeRunSync()
    println(resolved)
    resolved.size shouldBe 1
    resolved.head.isValid shouldBe true

    Files[IO].exists(resultPath).unsafeRunSync() shouldBe true

    val resultText = Files[IO]
      .readAll(resultPath, 1000)
      .fold(
        Vector
          .empty[Byte]
      )((acc, b) => acc :+ b)
      .flatMap(fs2.Stream.emits)
      .through(text.utf8Decode)
      .attempt
      .compile
      .last
      .unsafeRunSync()
      .get
      .right
      .get
    resultText shouldBe content

    Files[IO].deleteIfExists(resultPath).unsafeRunSync()
  }
}
