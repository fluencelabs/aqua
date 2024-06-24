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

package aqua

import aqua.files.FileModuleId
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{Ast, Parser, ParserError}

import cats.data.*
import cats.parse.LocationMap
import cats.{Comonad, Eval, Monad, Monoid, Order, ~>}

object SpanParser extends scribe.Logging {

  def parser: FileModuleId => String => ValidatedNec[ParserError[FileSpan.F], Ast[FileSpan.F]] =
    id =>
      source => {
        logger.trace(s"creating parser for $id...")
        val nat = new (Span.S ~> FileSpan.F) {
          override def apply[A](span: Span.S[A]): FileSpan.F[A] = {
            (
              FileSpan(id.file.absolute.toString, Eval.later(LocationMap(source)), span._1),
              span._2
            )
          }
        }
        val parser = Parser.natParser(Parser.spanParser, nat)(source)
        logger.trace("parser created")
        parser
      }
}
