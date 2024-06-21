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

package aqua.run

import aqua.parser.lexer.{CallArrowToken, CollectionToken, LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.raw.value.{CollectionRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*

import cats.data.Validated.{invalid, invalidNec, validNec}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.syntax.comonad.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Id, ~>}

case class CliFunc(name: String, args: List[ValueRaw] = Nil)

object CliFunc {

  private val spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = span.extract
  }

  def fromString(func: String): ValidatedNec[String, CliFunc] = {
    CallArrowToken.callArrow
      .parseAll(func.trim)
      .leftMap(error =>
        NonEmptyChain
          .fromNonEmptyList(error.expected)
          .map(_.context.mkString("\n"))
      )
      .toValidated
      .map(_.mapK(spanToId))
      .andThen(expr =>
        expr.args.traverse {
          case LiteralToken(value, ts) =>
            LiteralRaw(value, ts).validNec
          case VarToken(name) =>
            VarRaw(name.value, BottomType).validNec
          case CollectionToken(_, values) =>
            values.traverse {
              case LiteralToken(value, ts) =>
                LiteralRaw(value, ts).some
              case _ => none
            }.toValid(
              "Array elements can only be numbers, strings, or booleans."
            ).ensure(
              "If the argument is an array, then it must contain elements of the same type."
            )(_.distinctBy(_.`type`).size <= 1)
              .map(
                NonEmptyList
                  .fromList(_)
                  .map(l =>
                    CollectionRaw(
                      l,
                      ArrayType(
                        // FIXME: Type of Literal should always be a DataType
                        l.head.baseType.asInstanceOf[DataType]
                      )
                    )
                  )
                  .getOrElse(ValueRaw.Nil)
              )
              .toValidatedNec
          case CallArrowToken(_, _, _) =>
            "Function calls as arguments are not supported.".invalidNec
          case _ =>
            "Unsupported argument.".invalidNec
        }.map(args => CliFunc(expr.funcName.value, args))
      )
  }
}
