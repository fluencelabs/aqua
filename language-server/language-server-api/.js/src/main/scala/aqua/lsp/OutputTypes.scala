/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.lsp

import aqua.parser.lift.FileSpan

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.{UndefOr, undefined}

class CompilationResult(
  val errors: js.Array[ErrorInfo],
  val warnings: js.Array[WarningInfo] = js.Array(),
  val locations: js.Array[TokenLink] = js.Array(),
  val importLocations: js.Array[TokenImport] = js.Array(),
  val tokens: js.Array[ExprInfoJs] = js.Array()
) extends js.Object

class ExprInfoJs(val location: TokenLocation, val `type`: TypeJs) extends js.Object

class TokenLocation(
  val name: String,
  val startLine: Int,
  val startCol: Int,
  val endLine: Int,
  val endCol: Int
) extends js.Object

class TokenLink(val current: TokenLocation, val definition: TokenLocation) extends js.Object

class TokenImport(val current: TokenLocation, val path: String) extends js.Object

object TokenLocation {

  def fromSpan(span: FileSpan): Option[TokenLocation] = {
    val start = span.locationMap.value.toLineCol(span.span.startIndex)
    val end = span.locationMap.value.toLineCol(span.span.endIndex)

    for {
      startLC <- start
      endLC <- end
    } yield new TokenLocation(span.name, startLC._1, startLC._2, endLC._1, endLC._2)

  }
}

class ErrorInfo(
  val start: Int,
  val end: Int,
  val message: String,
  val location: UndefOr[String]
) extends js.Object {
  // Used to distinguish from WarningInfo in TS
  val infoType: String = "error"
}

object ErrorInfo {

  def apply(fileSpan: FileSpan, message: String): ErrorInfo = {
    val start = fileSpan.span.startIndex
    val end = fileSpan.span.endIndex
    new ErrorInfo(start, end, message, fileSpan.name)
  }

  def applyOp(start: Int, end: Int, message: String, location: Option[String]): ErrorInfo = {
    new ErrorInfo(start, end, message, location.getOrElse(undefined))
  }
}

class WarningInfo(
  val start: Int,
  val end: Int,
  val message: String,
  val location: UndefOr[String]
) extends js.Object {
  // Used to distinguish from ErrorInfo in TS
  val infoType: String = "warning"
}

object WarningInfo {

  def apply(fileSpan: FileSpan, message: String): WarningInfo = {
    val start = fileSpan.span.startIndex
    val end = fileSpan.span.endIndex
    new WarningInfo(start, end, message, fileSpan.name)
  }
}
