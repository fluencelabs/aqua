package aqua.lsp

import aqua.parser.lift.FileSpan

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.{UndefOr, undefined}

@JSExportAll
case class CompilationResult(
  errors: js.Array[ErrorInfo],
  warnings: js.Array[WarningInfo] = js.Array(),
  locations: js.Array[TokenLink] = js.Array(),
  importLocations: js.Array[TokenImport] = js.Array(),
  tokens: js.Array[TokenInfoJs] = js.Array()
)

@JSExportAll
case class TokenInfoJs(location: TokenLocation, `type`: String)

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
case class ErrorInfo(
  start: Int,
  end: Int,
  message: String,
  location: UndefOr[String]
) {
  // Used to distinguish from WarningInfo in TS
  val infoType: String = "error"
}

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

@JSExportAll
case class WarningInfo(
  start: Int,
  end: Int,
  message: String,
  location: UndefOr[String]
) {
  // Used to distinguish from ErrorInfo in TS
  val infoType: String = "warning"
}

object WarningInfo {

  def apply(fileSpan: FileSpan, message: String): WarningInfo = {
    val start = fileSpan.span.startIndex
    val end = fileSpan.span.endIndex
    WarningInfo(start, end, message, fileSpan.name)
  }
}
