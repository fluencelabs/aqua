package aqua.run

import aqua.parser.lexer.{CallArrowToken, CollectionToken, LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.raw.value.{CollectionRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.{ArrayType, BottomType}
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{invalid, invalidNel, validNel}
import cats.{Id, ~>}
import cats.syntax.traverse.*

case class CliFunc(name: String, args: List[ValueRaw] = Nil, ability: Option[String] = None)

object CliFunc {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  def fromString(func: String): Validated[NonEmptyList[String], CliFunc] = {
    CallArrowToken.callArrow.parseAll(func.trim) match {
      case Right(exprSpan) =>
        val expr = exprSpan.mapK(spanToId)

        val argsV = expr.args.collect {
          case LiteralToken(value, ts) =>
            validNel(LiteralRaw(value, ts))
          case VarToken(name, _) =>
            validNel(VarRaw(name.value, BottomType))
          case CollectionToken(_, values) =>
            val hasVariables = values.exists {
              case LiteralToken(_, _) => false
              case _ => true
            }
            if (!hasVariables) {
              val literals = values.collect { case LiteralToken(value, ts) =>
                LiteralRaw(value, ts)
              }
              val hasSameTypesOrEmpty =
                literals.isEmpty || literals.map(_.baseType).toSet.size == 1

              if (hasSameTypesOrEmpty) {
                validNel(
                  NonEmptyList
                    .fromList(literals)
                    .map(l => CollectionRaw(l, ArrayType(l.head.baseType)))
                    .getOrElse(ValueRaw.Nil)
                )
              } else
                invalidNel(
                  "If the argument is an array, then it must contain elements of the same type."
                )

            } else
              invalidNel(
                "Array arguments can only have numbers, strings, or booleans."
              )
          case CallArrowToken(_, _, _) =>
            invalidNel("Function calls as arguments are not supported.")
        }.sequence
        argsV.andThen(args =>
          validNel(CliFunc(expr.funcName.value, args, expr.ability.map(_.name)))
        )

      case Left(err) => invalid(err.expected.map(_.context.mkString("\n")))
    }
  }
}
