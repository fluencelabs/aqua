package aqua.lsp

import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.*
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.lsp.{LspContext, TokenInfo}
import aqua.semantics.{CompilerState, HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import cats.data.{NonEmptyChain, Validated}
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.{undefined, UndefOr}

import aqua.js.{FunctionDefJs, ServiceDefJs}

@JSExportAll
case class AquaFunction(funcDef: FunctionDefJs, script: String)

@JSExportAll
case class CompilationResult(
  services: js.Map[String, ServiceDefJs],
  functions: js.Map[String, AquaFunction]
)

@JSExportTopLevel("AquaLSP")
object AquaAPI extends App with Logging {

  @JSExport
  def compile(
    pathStr: String,
    imports: scalajs.js.Array[String]
  ): scalajs.js.Promise[CompilationResult] = {

    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    val path = Path(pathStr)
    val pathId = FileModuleId(path)
    val sources = new AquaFileSources[IO](path, imports.toList.map(Path.apply))
    val config = AquaCompilerConf()

    js.Promise.resolve(CompilationResult(js.Map.empty, js.Map.empty))

  }
}
