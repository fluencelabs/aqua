package aqua.lsp

import aqua.SpanParser
import aqua.compiler.FileIdString.given_FileId_String
import aqua.compiler.{AquaCompilerConf, AquaError, AquaSources}
import aqua.files.FileModuleId
import aqua.lsp.Utils.*
import aqua.parser.Parser
import aqua.parser.lexer.Token
import aqua.parser.lift.Span.S
import aqua.parser.lift.{FileSpan, Span}
import aqua.raw.ConstantRaw
import aqua.semantics.rules.locations.{DefinitionInfo, TokenLocation, VariableInfo}
import aqua.semantics.{RulesViolated, SemanticError}
import aqua.types.*

import cats.Id
import cats.data.*
import fs2.io.file.Path
import java.nio.file.Path as JPath
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileLSPSpec extends AnyFlatSpec with Matchers with Inside {

  extension (c: LspContext[FileSpan.F]) {

    def checkImportLocation(
      name: String,
      position: Int,
      code: String,
      targetFile: String
    ): Boolean = {
      (for {
        pos <- getByPosition(code, name, position).tapNone(
          fail(s"Didn't find definition of '$name'")
        )
      } yield {
        c.tokenPaths.exists { tip =>
          val (fileSpan, str) = tip.token.valueToken
          fileSpan.span.startIndex == pos._1 && fileSpan.span.endIndex == pos._2 && str == name && tip.path == targetFile
        }
      }).getOrElse(false)
    }
  }

  private def aquaSourceFile(src: Map[FileModuleId, String], imports: Map[FileModuleId, String]) = {
    new AquaSources[Id, String, FileModuleId] {

      override def sources: Id[ValidatedNec[String, Chain[(FileModuleId, String)]]] =
        Validated.validNec(Chain.fromSeq(src.toSeq))

      override def resolveImport(
        from: FileModuleId,
        imp: String
      ): Id[ValidatedNec[String, FileModuleId]] =
        Validated.validNec(FileModuleId(Path(imp)))

      override def load(file: FileModuleId): Id[ValidatedNec[String, String]] =
        Validated.fromEither(
          (imports ++ src)
            .get(file)
            .toRight(NonEmptyChain.one(s"Cannot load imported file $file"))
        )
    }
  }

  def toFMI(path: String): FileModuleId = FileModuleId(Path.fromNioPath(JPath.of(path)))

  def mapToFMI(src: Map[String, String]): Map[FileModuleId, String] = src.map { case (k, v) =>
    toFMI(k) -> v
  }

  def compileFileSpan(
    src: Map[FileModuleId, String],
    imports: Map[FileModuleId, String] = Map.empty
  ): ValidatedNec[AquaError[FileModuleId, String, FileSpan.F], Map[FileModuleId, LspContext[
    FileSpan.F
  ]]] = {
    LSPCompiler
      .compileToLsp[Id, String, FileModuleId, FileSpan.F](
        aquaSourceFile(src, imports),
        SpanParser.parser,
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
      .leftMap { ee =>
        println(ee)
        ee
      }
  }

  it should "return right tokens from 'import' and 'use' paths" in {
    val main =
      """aqua Import declares *
        |
        |use "first.aqua" as Export
        |import secondF from "second.aqua"
        |
        |""".stripMargin
    val src = mapToFMI(
      Map(
        "index.aqua" -> main
      )
    )

    val firstImport =
      """aqua First declares firstF
        |
        |func firstF() -> string:
        |  <- "firstStr"
        |
        |""".stripMargin

    val secondImport =
      """aqua Second declares secondF
        |
        |func secondF() -> string:
        |  <- "secondStr"
        |
        |""".stripMargin

    val imports = mapToFMI(
      Map(
        "first.aqua" ->
          firstImport,
        "second.aqua" -> secondImport
      )
    )

    val res = compileFileSpan(src, imports).toOption.get.values.head

    res.errors shouldBe empty
    res.checkImportLocation(
      "\"first.aqua\"",
      0,
      main,
      toFMI("first.aqua").file.toString
    ) shouldBe true
    res.checkImportLocation(
      "\"second.aqua\"",
      0,
      main,
      toFMI("second.aqua").file.toString
    ) shouldBe true
  }
}
