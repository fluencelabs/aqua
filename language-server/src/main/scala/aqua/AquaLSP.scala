package aqua

import scribe.Logging
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.concurrent.Future
import aqua.compiler.{
  AquaCompiler,
  AquaError,
  CompileError,
  CycleError,
  ImportErr,
  OutputError,
  ParserErr,
  ResolveImportsErr,
  SourcesErr
}
import aqua.parser.lift.{FileSpan, Span}
import aqua.io.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.model.transform.TransformConfig
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{HeaderError, RulesViolated, WrongAST}
import cats.data.NonEmptyChain
import fs2.io.file.{Files, Path}
import cats.data.Validated.{Invalid, Valid}
import scala.scalajs.js.JSConverters._
import concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.annotation.*

@JSExportAll
case class ErrorInfo(start: Int, end: Int, message: String, location: Option[String])

object ErrorInfo {

  def apply(fileSpan: FileSpan, message: String): ErrorInfo = {
    val start = fileSpan.span.startIndex
    val end = fileSpan.span.endIndex
    ErrorInfo(start, end, message, Some(fileSpan.name))
  }
}

@JSExportTopLevel("AquaLSP")
object AquaLSP extends App {

  @JSExport
  def sayHello(): Unit = {
    println("Hello world!")
  }

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
        ErrorInfo(0, 0, err.showForConsole, None) :: Nil
      case ResolveImportsErr(_, token, err) =>
        ErrorInfo(token.unit._1, err.showForConsole) :: Nil
      case ImportErr(token) =>
        ErrorInfo(token.unit._1, "Cannot resolve import") :: Nil
      case CycleError(modules) =>
        ErrorInfo(
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
            ErrorInfo(0, 0, "Semantic error: wrong AST", None) :: Nil

        }
      case OutputError(_, err) =>
        ErrorInfo(0, 0, err.showForConsole, None) :: Nil
    }
  }

  @JSExport
  def compile(
    pathStr: String,
    imports: scalajs.js.Array[String]
  ): scalajs.js.Promise[scalajs.js.Array[ErrorInfo]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    println("aqua files done")
    val sources = new AquaFileSources[IO](Path(pathStr), imports.toList.map(Path.apply))
    println("aqua sources done")
    val config = TransformConfig()
    println("config done")
    val proc = for {
      res <- AquaCompiler
        .compileToContext[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          config
        )
    } yield {
      println("compilation done")
      val result = res match {
        case Valid(_) => List.empty.toJSArray
        case Invalid(e: NonEmptyChain[AquaError[FileModuleId, AquaFileError, FileSpan.F]]) =>
          e.toNonEmptyList.toList.flatMap(errorToInfo).toJSArray
      }
      println("result: " + result)
      result
    }

    proc.unsafeToFuture().toJSPromise

  }
}
