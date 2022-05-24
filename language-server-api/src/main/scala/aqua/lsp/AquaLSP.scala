package aqua.lsp

import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.*
import aqua.parser.lexer.Token
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
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

@JSExportAll
case class CompilationResult(
  errors: js.Array[ErrorInfo],
  locations: js.Array[TokenLink]
)

@JSExportAll
case class TokenLocation(name: String, start: Int, end: Int)

@JSExportAll
case class TokenLink(current: TokenLocation, definition: TokenLocation)

object TokenLocation {

  def apply(span: FileSpan): TokenLocation = {
    TokenLocation(span.name, span.span.startIndex, span.span.endIndex)
  }
}

@JSExportAll
case class ErrorInfo(start: Int, end: Int, message: String, location: UndefOr[String])

object ErrorInfo {

  def apply(fileSpan: FileSpan, message: String): ErrorInfo = {
    val start = fileSpan.span.startIndex
    val end = fileSpan.span.endIndex
    ErrorInfo(start, end, message, fileSpan.name)
  }

  def applyOp(start: Int, end: Int, message: String, location: Option[String]): ErrorInfo = {
    ErrorInfo(start, end, message, location.getOrElse(undefined))
  }
}

@JSExportTopLevel("AquaLSP")
object AquaLSP extends App with Logging {

  def errorToInfo(error: AquaError[FileModuleId, AquaFileError, FileSpan.F]): List[ErrorInfo] = {
    error match {
      case ParserErr(err) =>
        err match {
          case BlockIndentError(indent, message) =>
            ErrorInfo(indent._1, message) :: Nil
          case ArrowReturnError(point, message) =>
            ErrorInfo(point._1, message) :: Nil
          case LexerError((span, e)) =>
            e.expected.toList
              .groupBy(_.offset)
              .map { case (offset, exps) =>
                val localSpan = Span(offset, offset + 1)
                val fSpan = FileSpan(span.name, span.locationMap, localSpan)
                val errorMessages = exps.flatMap(exp => ParserError.expectationToString(exp))
                val msg = s"${errorMessages.head}" :: errorMessages.tail.map(t => "OR " + t)
                (offset, ErrorInfo(fSpan, msg.mkString("\n")))
              }
              .toList
              .sortBy(_._1)
              .map(_._2)
              .reverse
        }
      case SourcesErr(err) =>
        ErrorInfo.applyOp(0, 0, err.showForConsole, None) :: Nil
      case ResolveImportsErr(_, token, err) =>
        ErrorInfo(token.unit._1, err.showForConsole) :: Nil
      case ImportErr(token) =>
        ErrorInfo(token.unit._1, "Cannot resolve import") :: Nil
      case CycleError(modules) =>
        ErrorInfo.applyOp(
          0,
          0,
          s"Cycle loops detected in imports: ${modules.map(_.file.fileName)}",
          None
        ) :: Nil
      case CompileError(err) =>
        err match {
          case RulesViolated(token, messages) =>
            ErrorInfo(token.unit._1, messages.mkString("\n")) :: Nil
          case HeaderError(token, message) =>
            ErrorInfo(token.unit._1, message) :: Nil
          case WrongAST(ast) =>
            ErrorInfo.applyOp(0, 0, "Semantic error: wrong AST", None) :: Nil

        }
      case OutputError(_, err) =>
        ErrorInfo.applyOp(0, 0, err.showForConsole, None) :: Nil
    }
  }

  @JSExport
  def compile(
    pathStr: String,
    imports: scalajs.js.Array[String]
  ): scalajs.js.Promise[CompilationResult] = {

    logger.debug(s"Compiling '$pathStr' with imports: $imports")

    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    val sources = new AquaFileSources[IO](Path(pathStr), imports.toList.map(Path.apply))
    val config = AquaCompilerConf()

    val proc = for {
      res <- AquaCompiler
        .compileToContext[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          config
        )
    } yield {
      logger.debug("Compilation done.")
      val result = res match {
        case Valid((state, _)) =>
          logger.debug("No errors on compilation.")
          CompilationResult(
            List.empty.toJSArray,
            state.toList
              .flatMap(s =>
                (s.names.locations ++ s.abilities.locations).flatMap { case (t, tInfo) =>
                  tInfo.definition match {
                    case None => Nil
                    case Some(d) =>
                      TokenLink(TokenLocation(t.unit._1), TokenLocation(d.unit._1)) :: Nil
                  }
                }
              )
              .toJSArray
          )
        case Invalid(e: NonEmptyChain[AquaError[FileModuleId, AquaFileError, FileSpan.F]]) =>
          val errors = e.toNonEmptyList.toList.flatMap(errorToInfo)
          logger.debug("Errors: " + errors.mkString("\n"))
          CompilationResult(errors.toJSArray, List.empty.toJSArray)
      }
      result
    }

    proc.unsafeToFuture().toJSPromise

  }
}
