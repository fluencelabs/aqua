package aqua.run

import aqua.parser.lexer.{CallArrowToken, CollectionToken, LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.raw.value.{CollectionRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.{ArrayType, BottomType}

import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.data.Validated.{invalid, invalidNec, validNec}
import cats.{~>, Id}
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.syntax.either.*
import cats.syntax.comonad.*
import cats.syntax.option.*

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
                  .map(l => CollectionRaw(l, ArrayType(l.head.baseType)))
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
