import aqua.AquaIO
import aqua.backend.Generated
import aqua.compiler.AquaCompiled
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.text
import org.scalatest.matchers.should.Matchers
import fs2.io.file.{Files, Path}
import org.scalatest.flatspec.AsyncFlatSpec

class SourcesSpec extends AsyncFlatSpec with Matchers {
  implicit val aquaIO: AquaIO[IO] = AquaFilesIO.summon[IO]

  "AquaFileSources" should "generate correct fileId with imports" in {
    val path = Path("cli/.jvm/src/test/test-dir/path-test")
    val importPath = path.resolve("imports")

    val sourceGen = new AquaFileSources[IO](path, importPath :: Nil)
    sourceGen.sources.map { result =>
      result.isValid shouldBe true

      val listResult = result
        .getOrElse(Chain.empty)
        .toList
        .map { case (fid, s) =>
          (fid.file.toString.split("/").last, s)
        }
        .sortBy(_._1) // sort cause different systems have different order of file reading

      val (id, importFile) = listResult(1)
      id shouldBe "index.aqua"
      importFile.nonEmpty shouldBe true

      val (importNearId, importFileNear) = listResult.head
      importNearId shouldBe "importNear.aqua"
      importFileNear.nonEmpty shouldBe true
    }.unsafeToFuture()
  }

  "AquaFileSources" should "throw an error if a source file doesn't exist" in {
    val path = Path("some/random/path")

    val sourceGen = new AquaFileSources[IO](path, Nil)

    sourceGen.sources.map(result => result.isInvalid shouldBe true).unsafeToFuture()
  }

  "AquaFileSources" should "throw an error if there is no import that is indicated in a source" in {
    val path = Path("cli/.jvm/src/test/test-dir")
    val importPath = path.resolve("random/import/path")

    val sourceGen = new AquaFileSources[IO](path, importPath :: Nil)
    sourceGen
      .resolveImport(FileModuleId(path.resolve("no-file.aqua")), "no/file")
      .map(result => result.isInvalid shouldBe true)
      .unsafeToFuture()
  }

  "AquaFileSources" should "find correct imports" in {
    val srcPath = Path("cli/.jvm/src/test/test-dir/index.aqua")
    val importPath = srcPath.resolve("imports")

    val sourceGen = new AquaFileSources[IO](srcPath, importPath :: Nil)

    (for {
      // should be found in importPath
      result <- sourceGen.resolveImport(FileModuleId(srcPath), "imports/import.aqua")
      exists <- {
        result.isValid shouldBe true
        val file = result.getOrElse(FileModuleId(Path("/some/random"))).file
        Files[IO].exists(file)
      }
      _ = exists shouldBe true

      // should be found near src file
      result2 <- sourceGen.resolveImport(FileModuleId(srcPath), "importNear.aqua")
      exists2 <- {
        result2.isValid shouldBe true
        val file2 = result2.getOrElse(FileModuleId(Path("/some/random"))).file
        Files[IO].exists(file2)
      }
      _ = exists2 shouldBe true
      // near src file but in another directory
      sourceGen2 = new AquaFileSources[IO](srcPath, Nil)
      result3 <- sourceGen2.resolveImport(FileModuleId(srcPath), "imports/import.aqua")
      exists3 <- {
        result3.isValid shouldBe true
        val file3 = result3.getOrElse(FileModuleId(Path("/some/random"))).file
        Files[IO].exists(file3)
      }
    } yield { exists3 shouldBe true }).unsafeToFuture()
  }

  "AquaFileSources" should "resolve correct path for target" in {
    val path = Path("cli/.jvm/src/test/test-dir")
    val filePath = path.resolve("some-dir/file.aqua")

    val targetPath = Path("/target/dir/")

    val sourceGen = new AquaFileSources[IO](path, Nil)

    val suffix = "_custom.super"

    sourceGen
      .resolveTargetPath(filePath, targetPath, suffix)
      .map { resolved =>
        resolved.isValid shouldBe true

        val targetFilePath = resolved.toOption.get
        targetFilePath.toString shouldBe "/target/dir/some-dir/file_custom.super"
      }
      .unsafeToFuture()
  }

  "AquaFileSources" should "write correct file with correct path" in {
    val path = Path("cli/.jvm/src/test/test-dir")
    val filePath = path.resolve("imports/import.aqua")

    val targetPath = path.resolve("target/")

    // clean up
    val resultPath = Path("cli/.jvm/src/test/test-dir/target/imports/import_hey.custom")
    (for {
      _ <- Files[IO].deleteIfExists(resultPath)
      sourceGen = new AquaFileSources[IO](path, Nil)
      content = "some random content"
      compiled = AquaCompiled[FileModuleId](
        FileModuleId(filePath),
        Seq(Generated("_hey.custom", content))
      )
      resolved <- sourceGen.write(targetPath)(compiled)
      _ = {
        resolved.size shouldBe 1
        resolved.head.isValid shouldBe true
      }
      exists <- Files[IO].exists(resultPath)
      _ = exists shouldBe true
      result <- Files[IO]
        .readAll(resultPath)
        .fold(
          Vector
            .empty[Byte]
        )((acc, b) => acc :+ b)
        .flatMap(fs2.Stream.emits)
        .through(text.utf8.decode)
        .attempt
        .compile
        .last
      resultText = result.get.right.get
      _ <- Files[IO].deleteIfExists(resultPath)
    } yield {
      resultText shouldBe content
    }).unsafeToFuture()
  }
}
