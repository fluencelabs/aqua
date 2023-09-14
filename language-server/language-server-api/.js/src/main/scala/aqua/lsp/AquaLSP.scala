package aqua.lsp

import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.*
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.raw.ConstantRaw
import aqua.semantics.{HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}
import cats.data.{NonEmptyChain, Validated}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.{UndefOr, undefined}

@JSExportAll
case class CompilationResult(
  errors: js.Array[ErrorInfo],
  locations: js.Array[TokenLink],
  importLocations: js.Array[TokenImport]
)

@JSExportAll
case class TokenLocation(name: String, startLine: Int, startCol: Int, endLine: Int, endCol: Int)

@JSExportAll
case class TokenLink(current: TokenLocation, definition: TokenLocation)

@JSExportAll
case class TokenImport(current: TokenLocation, path: String)

object TokenLocation {

  def fromSpan(span: FileSpan): Option[TokenLocation] = {
    val start = span.locationMap.value.toLineCol(span.span.startIndex)
    val end = span.locationMap.value.toLineCol(span.span.endIndex)

    for {
      startLC <- start
      endLC <- end
    } yield {
      TokenLocation(span.name, startLC._1, startLC._2, endLC._1, endLC._2)
    }

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

    val path = Path(pathStr)
    val pathId = FileModuleId(path)
    val sources = new AquaFileSources[IO](path, imports.toList.map(Path.apply))
    val config = AquaCompilerConf(ConstantRaw.defaultConstants(None))

    val proc = for {

      res <- LSPCompiler
        .compileToLsp[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          config
        )
    } yield {
      val fileRes: Validated[NonEmptyChain[
        AquaError[FileModuleId, AquaFileError, FileSpan.F]
      ], LspContext[FileSpan.F]] = res
        .andThen(
          _.getOrElse(
            pathId,
            invalidNec(
              SourcesErr(Unresolvable(s"Unexpected. No file $pathStr in compiler results"))
            )
          )
        )
        .andThen(
          _.get(pathId)
            .map(l => validNec(l))
            .getOrElse(
              invalidNec(
                SourcesErr(Unresolvable(s"Unexpected. No file $pathStr in compiler results"))
              )
            )
        )

      logger.debug("Compilation done.")

      def locationsToJs(
        locations: List[(Token[FileSpan.F], Token[FileSpan.F])]
      ): js.Array[TokenLink] = {
        locations.flatMap { case (from, to) =>
          
              val fromOp = TokenLocation.fromSpan(from.unit._1)
              val toOp = TokenLocation.fromSpan(to.unit._1)

              val link = for {
                from <- fromOp
                to <- toOp
              } yield {
                TokenLink(from, to)
              }

              if (link.isEmpty)
                logger.warn(s"Incorrect coordinates for token '${from.unit._1.name}'")

              link.toList
        }.toJSArray
      }

      def importsToTokenImport(imports: List[LiteralToken[FileSpan.F]]): js.Array[TokenImport] =
        imports.flatMap { lt =>
          val (span, str) = lt.valueToken
          val unquoted = str.substring(1, str.length - 1)
          TokenLocation.fromSpan(span).map(l => TokenImport(l, unquoted))
        }.toJSArray

      val result = fileRes match {
        case Valid(lsp) =>
          val errors = lsp.errors.map(CompileError[FileModuleId, AquaFileError, FileSpan.F]).flatMap(errorToInfo)
          errors match
            case Nil =>
              logger.debug("No errors on compilation.")
            case errs =>
              logger.debug("Errors: " + errs.mkString("\n"))

          CompilationResult(
            errors.toJSArray,
            locationsToJs(lsp.locations),
            importsToTokenImport(lsp.importTokens)
          )
        case Invalid(e: NonEmptyChain[AquaError[FileModuleId, AquaFileError, FileSpan.F]]) =>
          val errors = e.toNonEmptyList.toList.flatMap(errorToInfo)
          logger.debug("Errors: " + errors.mkString("\n"))
          CompilationResult(errors.toJSArray, List.empty.toJSArray, List.empty.toJSArray)
      }
      result
    }

    proc.unsafeToFuture().toJSPromise

  }
}
