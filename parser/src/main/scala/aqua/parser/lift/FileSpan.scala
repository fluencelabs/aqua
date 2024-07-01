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

package aqua.parser.lift

import cats.parse.{LocationMap, Parser => P, Parser0}
import cats.{Comonad, Eval}
import scala.language.implicitConversions

// TODO: rewrite FileSpan and Span under one trait
// TODO: move FileSpan to another package?
case class FileSpan(name: String, locationMap: Eval[LocationMap], span: Span) {

  /**
   * Focus on the line pointed by the span
   *
   * @param ctx How many lines to capture before and after the line
   * @return FileSpan.Focus
   */
  def focus(ctx: Int): Option[FileSpan.Focus] =
    span.
      focus(locationMap.value, ctx).map(FileSpan.Focus(name, locationMap, ctx, _))

  override def hashCode(): Int = (name, span).hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case FileSpan(n, _, s) => n == name && s == span
      case _ => false
    }
  }
}

object FileSpan {

  case class Focus(name: String, locationMap: Eval[LocationMap], ctx: Int, spanFocus: Span.Focus) {

    def toConsoleStr(
      messageType: String,
      msgs: List[String],
      onLeft: String,
      onRight: String = Console.RESET
    ): String =
      onLeft + "---- " + messageType + ": " + s"$name:${spanFocus.focus.number + 1}:${spanFocus.column + 1}" + onRight +
        spanFocus.toConsoleStr(
          msgs,
          onLeft,
          onRight
        )
  }

  type F[T] = (FileSpan, T)

  implicit object spanComonad extends Comonad[F] {
    override def extract[A](x: F[A]): A = x._2

    override def coflatMap[A, B](fa: F[A])(f: F[A] ⇒ B): F[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.copy(_2 = f(fa._2))
  }

}
