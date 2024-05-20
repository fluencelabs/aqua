package aqua.lsp

import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
import aqua.compiler.AquaWarning.CompileWarning
import aqua.compiler.{AquaError, AquaWarning}
import aqua.files.FileModuleId
import aqua.helpers.ext.Extension
import aqua.io.AquaFileError
import aqua.lsp.AquaLSP.logger
import aqua.parser.lexer.LiteralToken
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.rules.locations.{DefinitionInfo, TokenLocation as TokenLoc}
import aqua.semantics.{HeaderError, RulesViolated, SemanticWarning, WrongAST}

import cats.syntax.show.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scribe.Logging

object ResultHelper extends Logging {

  import aqua.types.Type.given

  def warningToInfo(
    warning: AquaWarning[FileSpan.F]
  ): List[WarningInfo] = warning match {
    case CompileWarning(SemanticWarning(token, messages)) =>
      WarningInfo(token.unit._1, messages.mkString("\n")) :: Nil
  }

  def errorToInfo(
    error: AquaError[FileModuleId, AquaFileError, FileSpan.F]
  ): List[ErrorInfo] = error match {
    case AquaParserError(err) =>
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
    case SourcesError(err) =>
      ErrorInfo.applyOp(0, 0, err.showForConsole, None) :: Nil
    case ResolveImportsError(_, token, err) =>
      ErrorInfo(token.unit._1, err.showForConsole) :: Nil
    case ImportError(token) =>
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
    case AirValidationError(errors) =>
      errors.toChain.toList.map(ErrorInfo.applyOp(0, 0, _, None))
  }

  private def tokensToJs(tokens: List[DefinitionInfo[FileSpan.F]]): js.Array[ExprInfoJs] =
    tokens.flatMap { ti =>
      TokenLocation.fromSpan(ti.token.unit._1).map { tl =>
        val typeDef = TypeJs.fromType(ti.`type`)
        ExprInfoJs(tl, typeDef)
      }
    }.toJSArray

  private def locationsToJs(
    locations: List[TokenLoc[FileSpan.F]]
  ): js.Array[TokenLink] =
    locations.flatMap { case TokenLoc(from, to) =>
      val fromOp = TokenLocation.fromSpan(from.unit._1)
      val toOp = TokenLocation.fromSpan(to.unit._1)

      val link = for {
        from <- fromOp
        to <- toOp
      } yield TokenLink(from, to)

      if (link.isEmpty)
        logger.warn(s"Incorrect coordinates for token '${from.unit._1.name}'")

      link.toList
    }.toJSArray

  private def importsToTokenImport(
    paths: List[TokenImportPath[FileSpan.F]]
  ): js.Array[TokenImport] =
    paths.flatMap { path =>
      val (span, _) = path.token.valueToken
      TokenLocation.fromSpan(span).map(l => TokenImport(l, path.path))
    }.toJSArray

  def lspToCompilationResult(lsp: LspContext[FileSpan.F]): CompilationResult = {
    val errors = lsp.errors.map(CompileError.apply).flatMap(errorToInfo)
    val warnings = lsp.warnings.map(CompileWarning.apply).flatMap(warningToInfo)

    errors match
      case Nil =>
        logger.debug("No errors on compilation.")
      case errs =>
        logger.debug("Errors: " + errs.mkString("\n"))

    val importTokens = importsToTokenImport(lsp.tokenPaths)

    CompilationResult(
      errors.toJSArray,
      warnings.toJSArray,
      locationsToJs(lsp.variables.locations),
      importTokens,
      tokensToJs(lsp.variables.definitions)
    )
  }
}
