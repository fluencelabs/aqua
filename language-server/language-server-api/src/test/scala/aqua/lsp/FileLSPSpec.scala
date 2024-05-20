package aqua.lsp

import aqua.SpanParser
import aqua.compiler.FileIdString.given
import aqua.compiler.{AquaCompilerConf, AquaError, AquaSources}
import aqua.files.FileModuleId
import aqua.lsp.Utils.*
import aqua.parser.lexer.Token
import aqua.parser.lift.Span.S
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{Ast, Parser, ParserError}
import aqua.raw.ConstantRaw
import aqua.semantics.rules.locations.{DefinitionInfo, TokenLocation, VariableInfo}
import aqua.semantics.{RulesViolated, SemanticError}
import aqua.types.*

import cats.data.*
import cats.parse.{LocationMap, Parser as P, Parser0}
import cats.{Comonad, Eval, Id, Monad, Monoid, Order, ~>}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileLSPSpec extends AnyFlatSpec with Matchers with Inside {

  extension (c: LspContext[FileSpan.F]) {

    def checkImportLocation(
      name: String,
      position: Int,
      code: String,
      sourceFile: String,
      targetFile: String
    ): Boolean = {
      (for {
        pos <- getByPosition(code, name, position).tapNone(
          fail(s"Didn't find definition of '$name'")
        )
      } yield {
        c.tokenPaths.exists { tip =>
          val (fileSpan, str) = tip.token.valueToken
          fileSpan.span.startIndex == pos._1 &&
          fileSpan.span.endIndex == pos._2 &&
          str == name &&
          tip.path == targetFile &&
          fileSpan.name == sourceFile
        }
      }).getOrElse(false)
    }
  }

  def spanStringParser: String => String => ValidatedNec[ParserError[FileSpan.F], Ast[FileSpan.F]] =
    id =>
      source => {
        val nat = new (Span.S ~> FileSpan.F) {
          override def apply[A](span: Span.S[A]): FileSpan.F[A] = {
            (
              FileSpan(id, Eval.later(LocationMap(source)), span._1),
              span._2
            )
          }
        }
        Parser.natParser(Parser.spanParser, nat)(source)
      }

  private def aquaSourceFile(src: Map[String, String], imports: Map[String, String]) = {
    new AquaSources[Id, String, String] {

      override def sources: Id[ValidatedNec[String, Chain[(String, String)]]] =
        Validated.validNec(Chain.fromSeq(src.toSeq))

      override def resolveImport(
        from: String,
        imp: String
      ): Id[ValidatedNec[String, String]] =
        Validated.validNec(imp)

      override def load(file: String): Id[ValidatedNec[String, String]] =
        Validated.fromEither(
          (imports ++ src)
            .get(file)
            .toRight(NonEmptyChain.one(s"Cannot load imported file $file"))
        )
    }
  }

  def compileFileSpan(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty
  ): ValidatedNec[AquaError[String, String, FileSpan.F], Map[String, LspContext[
    FileSpan.F
  ]]] = {
    LSPCompiler
      .compileToLsp[Id, String, String, FileSpan.F](
        aquaSourceFile(src, imports),
        spanStringParser,
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
  }

  it should "return right tokens from 'import' and 'use' paths" in {
    val main =
      """aqua Import declares *
        |
        |use "first.aqua" as Export
        |import secondF from "second"
        |
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
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

    val imports = Map(
      "first.aqua" ->
        firstImport,
      "second.aqua" -> secondImport
    )

    val res = compileFileSpan(src, imports).toOption.get.values.head

    res.errors shouldBe empty
    res.checkImportLocation(
      "\"first.aqua\"",
      0,
      main,
      "index.aqua",
      "first.aqua"
    ) shouldBe true
    res.checkImportLocation(
      "\"second\"",
      0,
      main,
      "index.aqua",
      "second.aqua"
    ) shouldBe true
  }
}
